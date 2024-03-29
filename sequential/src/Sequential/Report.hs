module Sequential.Report
  ( -- * Results
    Results (..),
    emptyResults,

    -- ** Cache
    readCache,
    saveCache,

    -- * Report
    Report (..),
    Stats (..),
    mkReport,
    saveReport,
  )
where

import Control.Exception (throwIO)
import Data.Aeson (AesonException (AesonException), FromJSON, ToJSON)
import Data.Aeson qualified as Asn
import Data.Set (Set)
import Data.Set qualified as Set
import GHC.Generics (Generic)
import Sequential.Cabal.Package (Package)
import Sequential.Utils qualified as Utils
import System.Directory.OsPath qualified as Dir

-- | Results of a run. This represents the current run __only__ i.e. it is not
-- the sum of the current run + cache.
data Results = MkResults
  { successes :: Set Package,
    failures :: Set Package,
    untested :: Set Package
  }
  deriving stock (Eq, Generic, Show)
  deriving anyclass (FromJSON, ToJSON)

-- | Empty results.
emptyResults :: Results
emptyResults = MkResults Set.empty Set.empty Set.empty

data Stats = MkStats
  { numSuccesses :: Int,
    successRate :: Int,
    numFailures :: Int,
    failureRate :: Int,
    numUntested :: Int,
    untestedRate :: Int
  }
  deriving stock (Eq, Generic, Show)
  deriving anyclass (FromJSON, ToJSON)

-- | Full report.
data Report = MkReport
  { results :: Results,
    stats :: Stats,
    startTime :: String,
    endTime :: String
  }
  deriving stock (Eq, Generic, Show)
  deriving anyclass (FromJSON, ToJSON)

-- | Derives a report from results.
mkReport :: Results -> String -> String -> Report
mkReport results startTime endTime =
  MkReport
    { results,
      stats =
        MkStats
          { numSuccesses,
            successRate,
            numFailures,
            failureRate,
            numUntested,
            untestedRate
          },
      startTime,
      endTime
    }
  where
    numSuccesses = length results.successes
    successRate = dv numSuccesses

    numFailures = length results.failures
    failureRate = dv numFailures

    numUntested = length results.untested
    untestedRate = dv numUntested

    numAllTested :: Double
    numAllTested = fromIntegral $ numSuccesses + numFailures + numUntested

    dv :: Int -> Int
    dv n = floor $ 100 * (fromIntegral n / numAllTested)

-- | Reads results data, if the cache exists.
readCache :: Bool -> IO (Maybe Results)
readCache colorLogs = do
  catchPathStr <- Utils.osPathToStringThrowM Utils.cachePath
  Dir.doesFileExist Utils.cachePath >>= \case
    False -> do
      Utils.putTimeInfoStr colorLogs $ "Cached results do not exist: " <> catchPathStr
      pure Nothing
    True -> do
      contents <- Utils.readBinaryFile Utils.cachePath
      case Asn.eitherDecodeStrict contents of
        Left err -> throwIO $ AesonException err
        Right r -> do
          Utils.putTimeInfoStr colorLogs $ "Using cached results: " <> catchPathStr
          pure $ Just r

-- | Saves the current progress data as the next prior run.
saveCache :: Results -> IO ()
saveCache results = do
  Dir.createDirectoryIfMissing False Utils.outputDir
  Utils.writeJson Utils.cachePath results

-- | Saves the report
saveReport :: Report -> IO ()
saveReport report = do
  Dir.createDirectoryIfMissing False Utils.outputDir
  Utils.writeJson Utils.reportPath report
