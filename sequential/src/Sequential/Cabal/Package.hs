module Sequential.Cabal.Package
  ( -- * Package
    Package (..),
    packageToCabal,
    cabalToPackage,
    prettyDep,
    pkgToDirName,
  )
where

import Data.Aeson (FromJSON, ToJSON)
import Data.Functor ((<&>))
import Data.String (IsString)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TEnc
import Data.Text.Encoding.Error (UnicodeException)
import Distribution.Fields (FieldLine (FieldLine))
import Distribution.Parsec (Position)
import GHC.Generics (Generic)
import Sequential.Utils qualified as Utils
import System.OsPath (OsPath)

-- | Wraps a package and version number e.g. 'pkg ==1.2.3'
newtype Package = MkPackage {unPackage :: Text}
  deriving stock (Eq, Generic, Ord, Show)
  deriving (FromJSON, IsString, ToJSON) via Text

-- | Transforms a package to text suitable to be added to the cabal file.
packageToCabal :: Position -> Package -> FieldLine Position
packageToCabal pos (MkPackage pkg) =
  FieldLine pos (TEnc.encodeUtf8 $ pkg <> ",")

-- | Transforms a FieldLine to a Package.
cabalToPackage :: FieldLine Position -> Either UnicodeException Package
cabalToPackage (FieldLine _ bs) = TEnc.decodeUtf8' bs <&> MkPackage . prettyDep

pkgToDirName :: Package -> IO OsPath
pkgToDirName p = Utils.textToOsPathThrowM p.unPackage

-- | Formats a 'dependency line' like "dep ==1.2.3," by stripping
-- leading/trailing whitespace and dropping the last char (comma).
prettyDep :: Text -> Text
prettyDep = T.dropEnd 1 . T.strip
