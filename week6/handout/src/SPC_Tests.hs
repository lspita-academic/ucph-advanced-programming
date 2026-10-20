module SPC_Tests (tests) where

import Control.Concurrent (threadDelay)
import Control.Monad (forM_)
import SPC
import Test.Tasty (TestTree, localOption, mkTimeout, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

tests :: TestTree
tests =
  localOption (mkTimeout 3000000) $
    testGroup
      "SPC"
      [ testCase "ping should pong" $ startSPC >>= pingSPC >>= (@?= 0),
        testCase "multiple pings in a row" $ do
          spc <- startSPC
          forM_ [0..10] $ \i -> pingSPC spc >>= (@?= i)
      ]
