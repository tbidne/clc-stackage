module Main (main) where

import Control.Exception (Exception (displayException), bracket, throwIO)
import Control.Monad (when)
import Data.Foldable (for_)
import Data.IORef (readIORef)
import GHC.Conc (setUncaughtExceptionHandler)
import Sequential.Cabal (CabalData (library, preamble))
import Sequential.Cabal qualified as Cabal
import Sequential.Env
  ( Env (cabalData, progress),
    Progress (failuresRef),
  )
import Sequential.Env qualified as Env
import Sequential.Runner qualified as Runner
import System.Exit (ExitCode (ExitFailure))

main :: IO ()
main = do
  setUncaughtExceptionHandler (putStrLn . displayException)

  bracket Env.setup Env.teardown $ \env -> do
    let pkgGroupsIdx = Runner.batchPackages env

    for_ pkgGroupsIdx $ \(pkgs, idx) -> do
      Cabal.writeMultiCabal
        env.cabalData.preamble
        env.cabalData.library
        pkgs
      Runner.buildClcStackage env idx pkgs

    numErrors <- length <$> readIORef env.progress.failuresRef
    when (numErrors > 0) $ throwIO $ ExitFailure 1
