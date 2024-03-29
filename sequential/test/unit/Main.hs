module Main (main) where

import Test.Tasty (defaultMain, testGroup)
import Unit.Sequential.Env qualified as Env
import Unit.Sequential.Report qualified as Report

main :: IO ()
main =
  defaultMain $
    testGroup
      "Unit"
      [ Env.tests,
        Report.tests
      ]
