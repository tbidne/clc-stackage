module Sequential.Cabal.CabalData
  ( CabalData (..),
    CabalLibrary (..),
  )
where

import Data.Sequence (Seq)
import Data.Sequence.NonEmpty (NESeq)
import Distribution.Fields (Field, SectionArg)
import Distribution.Fields.Field (Name)
import Distribution.Parsec.Position (Position)
import Sequential.Cabal.Package (Package)

-- | Cabal data.
data CabalData = MkCabalData
  { -- | Cabal file preamble.
    preamble :: Seq (Field Position),
    -- | Cabal library section.
    library :: CabalLibrary
  }
  deriving stock (Eq, Show)

-- | Cabal library section.
data CabalLibrary = MkCabalLibrary
  { -- | The library name.
    name :: Name Position,
    -- | The library sections.
    sectionArgs :: [SectionArg Position],
    -- | Other fields.
    other :: Seq (Field Position),
    -- | The top-level build-depends name (i.e. not the one within the
    -- 'if' guard for macos).
    buildDependsHeader :: Name Position,
    -- | The packages in the top-level buildDepends.
    packages :: NESeq Package
  }
  deriving stock (Eq, Show)
