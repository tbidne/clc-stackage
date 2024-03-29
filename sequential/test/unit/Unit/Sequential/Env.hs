{-# OPTIONS_GHC -Wno-missing-import-lists #-}

module Unit.Sequential.Env (tests) where

import Data.Set qualified as Set
import Sequential.Args (Args (retryFailures))
import Sequential.Env (Env (cache, cliArgs))
import Sequential.Env qualified as Env
import Sequential.Report
  ( Results (MkResults, failures, successes, untested),
  )
import Unit.Prelude

tests :: TestTree
tests =
  testGroup
    "Sequential.Env"
    [ testResults,
      newCacheTests
    ]

testResults :: TestTree
testResults = testCase "Retrieves expected results" $ do
  env <- mkEnv
  results <- Env.getResults env

  expected @=? results
  where
    expected =
      MkResults
        { successes = Set.fromList ["p1", "p2"],
          failures = Set.fromList ["p3", "p4"],
          untested = Set.fromList ["p5", "p6"]
        }

newCacheTests :: TestTree
newCacheTests =
  testGroup
    "resultsToNewCache"
    [ testEmptyCacheUpdate,
      testCacheUpdate,
      testCacheUpdateRetryFailures
    ]

testEmptyCacheUpdate :: TestTree
testEmptyCacheUpdate = testCase "Empty cache updated to new results" $ do
  env <- mkEnv
  let newResults =
        MkResults
          { successes = Set.fromList ["p1", "p2"],
            failures = Set.fromList ["p3", "p4"],
            untested = Set.fromList ["p5", "p6"]
          }
      newCache = Env.resultsToNewCache env newResults

  newResults @=? newCache

testCacheUpdate :: TestTree
testCacheUpdate = testCase "Cache updated to new results" $ do
  env <- mkEnv

  let oldCache =
        MkResults
          { successes = Set.fromList ["p1"],
            failures = Set.fromList ["p3"],
            untested = Set.fromList ["p2", "p4", "p5", "p6"]
          }
      env' = env {cache = Just oldCache}

  let newResults =
        MkResults
          { successes = Set.fromList ["p2"],
            failures = Set.fromList ["p4"],
            untested = Set.fromList ["p5", "p6"]
          }
      newCache = Env.resultsToNewCache env' newResults

  expected @=? newCache
  where
    expected =
      MkResults
        { successes = Set.fromList ["p1", "p2"],
          failures = Set.fromList ["p3", "p4"],
          untested = Set.fromList ["p5", "p6"]
        }

testCacheUpdateRetryFailures :: TestTree
testCacheUpdateRetryFailures = testCase "Cache updated to new results with retryFailures" $ do
  env <- mkEnv

  let oldCache =
        MkResults
          { successes = Set.fromList ["p1"],
            failures = Set.fromList ["p3"],
            untested = Set.fromList ["p2", "p4", "p5", "p6"]
          }
      args' = env.cliArgs {retryFailures = True}
      env' = env {cliArgs = args', cache = Just oldCache}

  let newResults =
        MkResults
          { successes = Set.fromList ["p2"],
            failures = Set.fromList ["p4"],
            untested = Set.fromList ["p5", "p6"]
          }
      newCache = Env.resultsToNewCache env' newResults

  expected @=? newCache
  where
    expected =
      MkResults
        { successes = Set.fromList ["p1", "p2"],
          failures = Set.fromList ["p4"],
          untested = Set.fromList ["p5", "p6"]
        }
