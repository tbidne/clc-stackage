{-# LANGUAGE QuasiQuotes #-}

module Sequential.Runner
  ( -- * Primary
    buildClcStackage,

    -- * Misc
    batchPackages,
  )
where

import Control.Exception (throwIO)
import Control.Monad (when)
import Data.IORef (modifyIORef')
import Data.List.NonEmpty qualified as NE
import Data.Sequence.NonEmpty (NESeq)
import Data.Sequence.NonEmpty qualified as NESeq
import Data.Set qualified as Set
import Data.Text qualified as T
import GHC.Exts (IsList (toList))
import Sequential.Args
  ( Args (batch, failFast, jobs, writeLogs),
    WriteLogs
      ( WriteLogsCurrent,
        WriteLogsNone,
        WriteLogsSaveFailures
      ),
  )
import Sequential.Args qualified as Args
import Sequential.Cabal.CabalData (CabalData (library), CabalLibrary (packages))
import Sequential.Cabal.Package (Package (unPackage))
import Sequential.Cabal.Package qualified as Package
import Sequential.Env
  ( Env
      ( cabalData,
        cliArgs,
        colorLogs,
        progress
      ),
    Progress
      ( failuresRef,
        successesRef
      ),
  )
import Sequential.Utils qualified as Utils
import System.Directory.OsPath qualified as Dir
import System.Exit (ExitCode (ExitFailure, ExitSuccess))
import System.OsPath (OsPath, osp, (</>))
import System.Process qualified as P

buildClcStackage :: Env -> Int -> NESeq Package -> IO ()
buildClcStackage env idx pkgs = do
  let buildNoLogs :: IO ExitCode
      buildNoLogs =
        (\(ec, _, _) -> ec) <$> P.readProcessWithExitCode "cabal" buildArgs ""

      buildLogs :: Bool -> IO ExitCode
      buildLogs saveFailures = do
        (dirPath, stdoutPath, stderrPath) <- createCurrentLogsDir

        Utils.withBinaryFileWriteMode stdoutPath $ \stdoutHandle ->
          Utils.withBinaryFileWriteMode stderrPath $ \stderrHandle -> do
            let createProc = P.proc "cabal" buildArgs
                createProc' =
                  createProc
                    { P.std_out = P.UseHandle stdoutHandle,
                      P.std_err = P.UseHandle stderrHandle,
                      P.close_fds = False
                    }

            ec <- P.withCreateProcess createProc' (\_ _ _ ph -> P.waitForProcess ph)
            case ec of
              -- Build dir will be overwritten next run anyway, so no need
              -- to do anything
              ExitSuccess -> pure ()
              -- Rename build dir 'current-build' to pkg specific name
              ExitFailure _ -> when saveFailures $ do
                pkgsDirPath <- getPkgGroupLogsDirPath pkgs

                -- If the log directory already exists (because, say, the user
                -- is retrying a package that already failed by manually moving
                -- the package name from cache.json's 'failures' to
                -- 'untested'), then we need to remove it first for this
                -- to succeed.
                pkgsDirExists <- Dir.doesDirectoryExist pkgsDirPath
                when pkgsDirExists (Dir.removeDirectoryRecursive pkgsDirPath)

                Dir.renamePath dirPath pkgsDirPath

            pure ec

  exitCode <-
    case env.cliArgs.writeLogs of
      Nothing -> buildLogs True
      Just WriteLogsNone -> buildNoLogs
      Just WriteLogsCurrent -> buildLogs False
      Just WriteLogsSaveFailures -> buildLogs True

  case exitCode of
    ExitSuccess -> do
      -- save results
      modifyIORef' env.progress.successesRef addPackages
      Utils.putTimeSuccessStr env.colorLogs msg
    ExitFailure _ -> do
      -- save results
      modifyIORef' env.progress.failuresRef addPackages
      Utils.putTimeErrStr env.colorLogs msg

      -- throw error if fail fast
      when env.cliArgs.failFast $ throwIO exitCode
  where
    msg =
      T.unpack $
        mconcat
          [ T.pack $ show idx,
            ": ",
            T.intercalate ", " ((.unPackage) <$> pkgsList)
          ]
    pkgsList = toList $ NESeq.toSeq pkgs
    pkgsSet = Set.fromList pkgsList

    addPackages = Set.union pkgsSet

    buildArgs =
      [ "build",
        "clc-stackage"
      ]
        ++ cabalVerboseArg
        ++ jobsArg

    cabalVerboseArg = toArgs Args.cabalVerbosityToArg env.cliArgs.cabalVerbosity

    jobsArg = toArgs Args.jobsToArg env.cliArgs.jobs

    toArgs :: (a -> b) -> Maybe a -> [b]
    toArgs f = maybe [] (\x -> [f x])

batchPackages :: Env -> NESeq (NESeq Package, Int)
batchPackages env = pkgGroupsIdx
  where
    pkgGroups = case env.cliArgs.batch of
      Nothing -> NESeq.singleton <$> env.cabalData.library.packages
      Just n -> NESeq.chunksOf n env.cabalData.library.packages

    -- NOTE: NE.fromList is obviously unsafe in general, but here it is fine
    -- as 'n := length pkgGroups > 0' and [1 .. n] is non-empty such n.
    indexes = NESeq.fromList $ NE.fromList $ reverse [1 .. length pkgGroups]
    pkgGroupsIdx = NESeq.zip pkgGroups indexes

createCurrentLogsDir :: IO (OsPath, OsPath, OsPath)
createCurrentLogsDir = do
  let dirPath = Utils.logsDir </> [osp|current-build|]
      stdoutPath = dirPath </> [osp|stdout.log|]
      stderrPath = dirPath </> [osp|stderr.log|]

  Dir.createDirectoryIfMissing True dirPath
  pure (dirPath, stdoutPath, stderrPath)

-- Name the dir after the first package in the group
getPkgGroupLogsDirPath :: NESeq Package -> IO OsPath
getPkgGroupLogsDirPath pkgs = do
  dirName <- Package.pkgToDirName . NESeq.head $ pkgs
  pure $ Utils.logsDir </> dirName
