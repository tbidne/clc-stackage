module Sequential.Env
  ( -- * Main
    Env (..),
    Progress (..),
    setup,
    teardown,

    -- * Misc
    getResults,
    resultsToNewCache,
  )
where

import Control.Exception (throwIO)
import Control.Monad (unless)
import Data.Foldable (Foldable (foldl'))
import Data.IORef (IORef, newIORef, readIORef)
import Data.Maybe (fromMaybe)
import Data.Sequence qualified as Seq
import Data.Sequence.NonEmpty (NESeq ((:<||)))
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Time.LocalTime (LocalTime)
import Sequential.Args
  ( Args
      ( noCache,
        noCleanup,
        retryFailures
      ),
  )
import Sequential.Args qualified as Args
import Sequential.Cabal qualified as Cabal
import Sequential.Cabal.CabalData
  ( CabalData (library),
    CabalLibrary (packages),
  )
import Sequential.Cabal.Package (Package)
import Sequential.Report
  ( Report (stats),
    Results (MkResults, failures, successes, untested),
    Stats (numFailures, numSuccesses, numUntested),
  )
import Sequential.Report qualified as Report
import Sequential.Utils qualified as Utils
import System.Console.Pretty (supportsPretty)
import System.Directory.OsPath qualified as Dir
import System.Exit (ExitCode (ExitSuccess))

data Progress = MkProgress
  { -- | Dependencies that built successfully.
    successesRef :: IORef (Set Package),
    -- | Dependencies that failed to build.
    failuresRef :: IORef (Set Package)
  }

-- | Args used for building all packages.
data Env = MkEnv
  { -- | CLI args.
    cliArgs :: Args,
    -- | Cabal file data.
    cabalData :: CabalData,
    -- | Status from previous run.
    cache :: Maybe Results,
    -- | Whether to color the logs.
    colorLogs :: Bool,
    -- | Status for this run.
    progress :: Progress,
    -- | Start time.
    startTime :: LocalTime
  }

-- | Creates an environment based on cli args and cache data.
setup :: IO Env
setup = do
  startTime <- Utils.getLocalTime
  cliArgs <- Args.getArgs

  cabalData <- Cabal.readCabal
  successesRef <- newIORef Set.empty
  failuresRef <- newIORef Set.empty

  colorLogs <-
    case cliArgs.colorLogs of
      Just False -> pure False
      Just True -> pure True
      Nothing -> supportsPretty

  cache <-
    if cliArgs.noCache
      then pure Nothing
      else Report.readCache colorLogs

  -- Update dependencies if we have cached results.
  pkgsToBuild <- case cache of
    Nothing -> pure cabalData.library.packages
    Just oldResults -> do
      oldFailures <-
        if cliArgs.retryFailures
          then do
            -- Remove previous errors if we are retrying.
            Utils.removeDirectoryRecursiveIfExists Utils.logsDir
            pure oldResults.failures
          else pure Set.empty

      let untested = oldResults.untested
          toBuild = Set.toList $ Set.union untested oldFailures

      case toBuild of
        (p : ps) -> pure (p :<|| Seq.fromList ps)
        [] -> do
          Utils.putTimeInfoStr colorLogs "Cache exists but has no packages to test."
          throwIO ExitSuccess

  let cabalLibrary = cabalData.library {packages = pkgsToBuild}
      cabalData' = cabalData {library = cabalLibrary}
      progress =
        MkProgress
          { successesRef,
            failuresRef
          }

  Dir.renameFile Cabal.cabalPath Cabal.tmpCabalPath
  pure $
    MkEnv
      { cliArgs,
        cabalData = cabalData',
        cache,
        colorLogs,
        progress,
        startTime
      }

-- | Prints summary and writes results to disk.
teardown :: Env -> IO ()
teardown env = do
  endTime <- Utils.getLocalTime
  unless env.cliArgs.noCleanup $
    Dir.renameFile Cabal.tmpCabalPath Cabal.cabalPath

  results <- getResults env
  let report =
        Report.mkReport
          results
          (Utils.formatLocalTime env.startTime)
          (Utils.formatLocalTime endTime)

  unless env.cliArgs.noCache (updateCache env results)

  Report.saveReport report

  putStrLn $
    unlines
      [ "",
        "",
        Utils.colorGreen env.colorLogs $ "- Successes: " <> successStr report,
        Utils.colorRed env.colorLogs $ "- Failures:  " <> failureStr report,
        Utils.colorMagenta env.colorLogs $ "- Untested:  " <> untestedStr report,
        "",
        Utils.colorBlue env.colorLogs $ "- Start: " <> report.startTime,
        Utils.colorBlue env.colorLogs $ "- End:   " <> report.endTime
      ]
  where
    successStr r = fmtPercent r.stats.numSuccesses r.stats.successRate
    failureStr r = fmtPercent r.stats.numFailures r.stats.failureRate
    untestedStr r = fmtPercent r.stats.numUntested r.stats.untestedRate

    fmtPercent n p =
      mconcat
        [ show n,
          " (",
          show p,
          "%)"
        ]

getResults :: Env -> IO Results
getResults env = do
  currSuccesses :: Set Package <- readIORef env.progress.successesRef
  currFailures :: Set Package <- readIORef env.progress.failuresRef

  let currAllTested = Set.union currSuccesses currFailures

      currUntested = foldl' addUntested Set.empty env.cabalData.library.packages

      addUntested acc d =
        if Set.member d currAllTested
          then acc
          else Set.insert d acc

  pure $
    MkResults
      { successes = currSuccesses,
        failures = currFailures,
        untested = currUntested
      }

updateCache :: Env -> Results -> IO ()
updateCache env = Report.saveCache . resultsToNewCache env

resultsToNewCache :: Env -> Results -> Results
resultsToNewCache env newResults = newCache
  where
    oldCache = fromMaybe Report.emptyResults env.cache
    newCache =
      MkResults
        { -- Successes is append-only.
          successes = Set.union oldCache.successes newResults.successes,
          -- Untested is always the latest as each cached run always adds the
          -- previous untested to the pkgsToBuild.
          untested = newResults.untested,
          failures =
            if env.cliArgs.retryFailures
              then -- Case 1: Retrying previous failures: Then the cache's
              --              results are out-of-date i.e. old failures might
              --              have passed or become untested. Only save the new
              --              results.
                newResults.failures
              else -- Case 2: No retry: total failures are previous + new.
                Set.union oldCache.failures newResults.failures
        }
