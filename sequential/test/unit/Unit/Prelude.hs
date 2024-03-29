module Unit.Prelude
  ( module X,
    mkEnv,
  )
where

import Data.IORef (newIORef)
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.Sequence qualified as Seq
import Data.Sequence.NonEmpty qualified as NESeq
import Data.Set qualified as Set
import Data.Time.LocalTime (LocalTime (LocalTime), midday)
import Distribution.Fields (Name (Name))
import Distribution.Parsec.Position (Position (Position))
import Sequential.Args
  ( Args
      ( MkArgs,
        batch,
        cabalVerbosity,
        colorLogs,
        failFast,
        jobs,
        noCache,
        noCleanup,
        retryFailures,
        writeLogs
      ),
  )
import Sequential.Cabal.CabalData (CabalData, CabalLibrary)
import Sequential.Cabal.Parser
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
import Sequential.Env
  ( Env
      ( MkEnv,
        cabalData,
        cache,
        cliArgs,
        colorLogs,
        progress,
        startTime
      ),
    Progress (MkProgress),
  )
import Test.Tasty as X (TestTree, testGroup)
import Test.Tasty.HUnit as X (testCase, (@=?))

mkEnv :: IO Env
mkEnv = do
  successesRef <- newIORef (Set.fromList ["p1", "p2"])
  failuresRef <- newIORef (Set.fromList ["p3", "p4"])

  pure $
    MkEnv
      { cliArgs = mkArgs,
        cabalData = mkCabalData,
        cache = Nothing,
        colorLogs = False,
        progress = MkProgress successesRef failuresRef,
        startTime = mkLocalTime
      }

mkArgs :: Args
mkArgs =
  MkArgs
    { batch = Nothing,
      cabalVerbosity = Nothing,
      colorLogs = Nothing,
      failFast = False,
      jobs = Nothing,
      noCache = False,
      noCleanup = False,
      retryFailures = False,
      writeLogs = Nothing
    }

mkCabalData :: CabalData
mkCabalData =
  MkCabalData
    { preamble = Seq.empty,
      library = mkCabalLibrary
    }

mkCabalLibrary :: CabalLibrary
mkCabalLibrary =
  MkCabalLibrary
    { name = Name (Position 0 0) "",
      sectionArgs = [],
      other = Seq.empty,
      buildDependsHeader = Name (Position 0 0) "",
      packages = NESeq.fromList ("p1" :| ["p2", "p3", "p4", "p5", "p6"])
    }

mkLocalTime :: LocalTime
mkLocalTime = LocalTime (toEnum 59_000) midday
