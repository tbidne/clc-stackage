{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS_GHC -Wno-missing-import-lists #-}

module Unit.Sequential.Report (tests) where

import Data.Set qualified as Set
import Sequential.Report
  ( Report (MkReport, endTime, results, startTime, stats),
    Results (MkResults, failures, successes, untested),
    Stats
      ( MkStats,
        failureRate,
        numFailures,
        numSuccesses,
        numUntested,
        successRate,
        untestedRate
      ),
  )
import Sequential.Report qualified as Report
import Sequential.Utils qualified as Utils
import System.OsPath (osp)
import Unit.Prelude

tests :: TestTree
tests =
  testGroup
    "Sequential.Report"
    [ testMkReport,
      testResultJsonEncode,
      testReportJsonEncode
    ]

testMkReport :: TestTree
testMkReport = testCase "Creates a report" $ do
  let report = Report.mkReport results "start" "end"

  expected @=? report
  where
    results =
      MkResults
        { successes = Set.fromList ["p1", "p2", "p3", "p4", "p5"],
          failures = Set.fromList ["p4", "p5"],
          untested = Set.fromList ["p6", "p7", "p8"]
        }

    expected =
      MkReport
        { results = results,
          stats =
            MkStats
              { numSuccesses = 5,
                successRate = 50,
                numFailures = 2,
                failureRate = 20,
                numUntested = 3,
                untestedRate = 30
              },
          startTime = "start",
          endTime = "end"
        }

testResultJsonEncode :: TestTree
testResultJsonEncode = testCase "Encodes Result to JSON" $ do
  let json = Utils.encodePretty results

  expected <- Utils.readBinaryFile [osp|test/unit/data/result.json|]

  expected @=? json
  where
    results =
      MkResults
        { successes = Set.fromList ["p1", "p2", "p3", "p4", "p5"],
          failures = Set.fromList ["p4", "p5"],
          untested = Set.fromList ["p6", "p7", "p8"]
        }

testReportJsonEncode :: TestTree
testReportJsonEncode = testCase "Encodes Report to JSON" $ do
  let json = Utils.encodePretty report

  expected <- Utils.readBinaryFile [osp|test/unit/data/report.json|]

  expected @=? json
  where
    results =
      MkResults
        { successes = Set.fromList ["p1", "p2", "p3", "p4", "p5"],
          failures = Set.fromList ["p4", "p5"],
          untested = Set.fromList ["p6", "p7", "p8"]
        }

    report =
      MkReport
        { results = results,
          stats =
            MkStats
              { numSuccesses = 5,
                successRate = 50,
                numFailures = 2,
                failureRate = 20,
                numUntested = 3,
                untestedRate = 30
              },
          startTime = "start",
          endTime = "end"
        }
