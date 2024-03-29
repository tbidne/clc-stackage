{-# LANGUAGE QuasiQuotes #-}

module Sequential.Cabal.Parser
  ( CabalData (..),
    CabalLibrary (..),
    readCabal,

    -- * Misc
    cabalPath,
    ParseCabalException (..),
  )
where

import Control.Applicative (Alternative ((<|>)))
import Control.Exception (Exception (displayException), throwIO)
import Data.Sequence (Seq (Empty, (:<|)))
import Data.Sequence qualified as Seq
import Data.Sequence.NonEmpty (NESeq ((:<||)))
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TEnc
import Distribution.Fields (Field (Field, Section), Name (Name), SectionArg)
import Distribution.Fields qualified as Fields
import Distribution.Parsec (Position)
import Sequential.Cabal.CabalData
  ( CabalData
      ( MkCabalData,
        library,
        preamble
      ),
    CabalLibrary
      ( MkCabalLibrary,
        buildDependsHeader,
        name,
        other,
        packages,
        sectionArgs
      ),
  )
import Sequential.Cabal.Package qualified as Package
import Sequential.Utils qualified as Utils
import System.OsPath (OsPath)
import System.OsPath qualified as OsPath

-- | Reads the clc-stackage cabal file. Returns @(preamble, packages)@, where
-- @packages@ is a list of all build-depends lines like "dep ==1.2.3,", and
-- @preamble@ is everything before it. I.e. we parse the cabal file like:
--
-- @
--    preamble
--    packages
-- @
--
-- This is so that we can try building
--
-- @
--    preamble
--    p
-- @
--
-- for all @p@ in @packages@.
--
-- Note that the MacOS specific build-depends is considered part of the
-- preamble i.e. no care is taken to include it in this logic.
readCabal :: IO CabalData
readCabal = do
  contents <- Utils.readBinaryFile cabalPath

  let throwCabalEx :: String -> [Field Position] -> IO a
      throwCabalEx msg fields =
        throwIO $
          MkParseCabalException
            msg
            cabalPath
            contentsTxt
            fields
        where
          contentsTxt = case TEnc.decodeUtf8' contents of
            Left _ -> T.pack $ show contents
            Right t -> t

  case Fields.readFields contents of
    Left err -> throwCabalEx (show err) []
    Right fields -> do
      let parseAcc = foldMap toParseAcc fields

      (preamble, library) <- do
        let throwCabalEx' :: String -> IO a
            throwCabalEx' msg = throwCabalEx msg fields

        lib <- case parseAcc.library of
          Nothing -> throwCabalEx' "No library found."
          Just parseLib -> do
            (other, buildDependsHeader, packages) <-
              case parseLib.buildDepends of
                -- NOTE: Expecting only 1 build-depends as the macos one is
                -- parsed as part of the "if" section.
                (Field n subFields :<| Empty) -> do
                  pkgs <- case traverse Package.cabalToPackage subFields of
                    Left ex ->
                      throwCabalEx'
                        ("Could not decode build-depends to UTF-8: " ++ displayException ex)
                    Right [] ->
                      throwCabalEx' "No entries found in top-level build-depends."
                    Right (p : ps) -> pure $ p :<|| Seq.fromList ps

                  pure
                    ( parseLib.other,
                      n,
                      pkgs
                    )
                Empty -> throwCabalEx' "Found 0 build-depends, expected 1."
                _ -> throwCabalEx' "Found > 1 build-depends, expected 1."

            pure $
              MkCabalLibrary
                { name = parseLib.name,
                  sectionArgs = parseLib.sectionArgs,
                  other,
                  buildDependsHeader,
                  packages
                }

        pure (parseAcc.preamble, lib)

      pure $
        MkCabalData
          { preamble,
            library
          }
  where
    toParseAcc :: Field Position -> ParseAcc
    toParseAcc (Section name@(Name _ "library") sectionArgs subFields) =
      let (other, buildDepends) = foldMap toParseLibrary subFields
       in MkParseAcc
            { preamble = Seq.empty,
              library =
                Just $
                  MkParseLibrary
                    { name,
                      sectionArgs,
                      other,
                      buildDepends
                    }
            }
    toParseAcc f =
      MkParseAcc
        { preamble = Seq.singleton f,
          library = Nothing
        }

    toParseLibrary ::
      Field Position ->
      (Seq (Field Position), Seq (Field Position))
    toParseLibrary f@(Field (Name _ "build-depends") _) = (Seq.empty, Seq.singleton f)
    toParseLibrary f = (Seq.singleton f, Seq.empty)

data ParseLibrary = MkParseLibrary
  { name :: Name Position,
    sectionArgs :: [SectionArg Position],
    other :: Seq (Field Position),
    buildDepends :: Seq (Field Position)
  }

data ParseAcc = MkParseAcc
  { preamble :: Seq (Field Position),
    library :: Maybe ParseLibrary
  }

instance Semigroup ParseAcc where
  l <> r =
    MkParseAcc
      (l.preamble <> r.preamble)
      (l.library <|> r.library)

instance Monoid ParseAcc where
  mempty = MkParseAcc Seq.empty Nothing

-- | Path to clc-stackage.cabal.
cabalPath :: OsPath
cabalPath = [OsPath.osp|clc-stackage.cabal|]

data ParseCabalException = MkParseCabalException
  { -- | Error message.
    message :: String,
    -- | File path.
    filePath :: OsPath,
    -- | Cabal source.
    source :: Text,
    -- | Parsed fields.
    fields :: [Field Position]
  }
  deriving stock (Eq, Show)

instance Exception ParseCabalException where
  displayException ex =
    mconcat
      [ "Error parsing cabal file '",
        Utils.osPathToStringShow ex.filePath,
        "': ",
        ex.message,
        "\n\n*** SOURCE ***\n\n",
        T.unpack ex.source,
        "\n\n*** FIELDS ***\n\n",
        prettyFields
      ]
    where
      prettyFields =
        unlines
          . fmap show
          $ ex.fields
