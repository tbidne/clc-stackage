{-# LANGUAGE QuasiQuotes #-}

module Sequential.Cabal
  ( -- * Types
    CabalData (..),
    CabalLibrary (..),

    -- * Main functions
    Parser.readCabal,
    writeSingleCabal,
    writeMultiCabal,

    -- * Cabal paths
    Parser.cabalPath,
    tmpCabalPath,
  )
where

import Data.ByteString (ByteString)
import Data.Foldable (Foldable (foldl', toList))
import Data.Sequence (Seq ((:|>)))
import Data.Sequence.NonEmpty (NESeq)
import Data.Sequence.NonEmpty qualified as NESeq
import Data.Text qualified as T
import Data.Text.Encoding qualified as TEnc
import Distribution.Fields (Field (Field, Section), Name (Name))
import Distribution.Fields qualified as Fields
import Distribution.Fields.Field (FieldLine)
import Distribution.Parsec (Position (Position))
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
import Sequential.Cabal.Package (Package)
import Sequential.Cabal.Package qualified as Package
import Sequential.Cabal.Parser qualified as Parser
import Sequential.Utils qualified as Utils
import System.OsPath (OsPath)
import System.OsPath qualified as OsPath

-- | Writes a cabal file for a dependency list formatted as "dep ==1.2.3,".
writeMultiCabal ::
  -- | The cabal preamble before the library.
  Seq (Field Position) ->
  -- | The library section.
  CabalLibrary ->
  -- | The dependency packages e.g. [dep ==1.2.3, ...].
  NESeq Package ->
  IO ()
writeMultiCabal preamble library packages =
  Utils.writeBinaryFile Parser.cabalPath newCabalSrc
  where
    (Name (Position headerRow headerCol) _) = library.buildDependsHeader

    newCabalSrc :: ByteString
    newCabalSrc = encodeCabalFields (toList newCabalFields)

    newCabalFields :: Seq (Field Position)
    newCabalFields = preamble :|> newLibrary

    newLibrary :: Field Position
    newLibrary =
      Section
        library.name
        library.sectionArgs
        (toList $ library.other :|> newBuildDepends)

    newBuildDepends :: Field Position
    newBuildDepends =
      Field
        library.buildDependsHeader
        (pkgsToFieldLines packages)

    pkgCol = headerCol + 2

    pkgsToFieldLines :: NESeq Package -> [FieldLine Position]
    pkgsToFieldLines =
      reverse
        . fst
        . foldl' pkgToFieldLine ([], headerRow + 1)

    pkgToFieldLine ::
      ([FieldLine Position], Int) ->
      Package ->
      ([FieldLine Position], Int)
    pkgToFieldLine (acc, !row) pkg =
      (Package.packageToCabal pos pkg : acc, row + 1)
      where
        pos = Position row pkgCol

-- | Writes a cabal file for a single dependency formatted as "dep ==1.2.3,".
writeSingleCabal ::
  -- | The cabal preamble before the library.
  Seq (Field Position) ->
  -- | The library section.
  CabalLibrary ->
  -- | The package to build.
  Package ->
  IO ()
writeSingleCabal preamble library =
  writeMultiCabal preamble library . NESeq.singleton

-- | Path to clc-stackage.cabal backup.
tmpCabalPath :: OsPath
tmpCabalPath = [OsPath.osp|clc-stackage.cabal.bkp|]

encodeCabalFields :: [Field Position] -> ByteString
encodeCabalFields =
  TEnc.encodeUtf8
    . T.pack
    . Fields.showFields (const Fields.NoComment)
    . Fields.fromParsecFields
