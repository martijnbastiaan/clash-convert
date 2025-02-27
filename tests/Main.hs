module Main (main) where

import Test.Tasty (TestTree, defaultMain, testGroup)
import Prelude

import qualified Tests.Clash.Convert
import qualified Tests.Clash.MaybeConvert

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests =
  testGroup
    "Tests"
    [ Tests.Clash.Convert.tests
    , Tests.Clash.MaybeConvert.tests
    ]
