module SPC_Tests (tests) where

import Control.Concurrent (threadDelay)
import Control.Monad (forM_)
import SPC
import Test.Tasty (TestTree, localOption, mkTimeout, testGroup)
import Test.Tasty.HUnit (assertBool, testCase, (@?=))

tests :: TestTree
tests =
  localOption (mkTimeout 3000000) $
    testGroup
      "SPC"
      [ testCase "ping should pong" $ startSPC >>= pingSPC >>= (@?= 0),
        testCase "multiple pings in a row" $ do
          spc <- startSPC
          forM_ [0 .. 10] $ \i -> pingSPC spc >>= (@?= i),
        testCase "job add should not fail" $ do
          spc <- startSPC
          _ <- jobAdd spc $ Job {jobAction = pure (), jobMaxSeconds = 0}
          pure (),
        testCase "job ids should be unique" $ do
          spc <- startSPC
          id1 <- jobAdd spc $ Job {jobAction = pure (), jobMaxSeconds = 0}
          id2 <- jobAdd spc $ Job {jobAction = pure (), jobMaxSeconds = 0}
          assertBool "job ids should differ" $ id1 /= id2
      ]
