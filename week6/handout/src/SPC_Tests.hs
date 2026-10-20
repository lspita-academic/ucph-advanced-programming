module SPC_Tests (tests) where

import Control.Concurrent (threadDelay)
import Control.Monad (forM_)
import SPC
import Test.Tasty (TestTree, localOption, mkTimeout, testGroup)
import Test.Tasty.HUnit (assertBool, testCase, (@?=))

emptyJob :: Job
emptyJob = Job {jobAction = pure (), jobMaxSeconds = 0}

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
          _ <- jobAdd spc emptyJob
          pure (),
        testCase "job ids should be unique" $ do
          spc <- startSPC
          id1 <- jobAdd spc emptyJob
          id2 <- jobAdd spc emptyJob
          assertBool "job ids should differ" $ id1 /= id2,
        testCase "new job should start as pending" $ do
          spc <- startSPC
          jobId <- jobAdd spc emptyJob
          jobStatus spc jobId >>= (@?= Just JobPending),
        testCase "non-existing job should have no status" $ do
          spc <- startSPC
          jobId <- jobAdd spc emptyJob
          let missingJobId = succ jobId
          jobStatus spc missingJobId >>= (@?= Nothing),
        testCase "pending job should be cancelled" $ do
          spc <- startSPC
          jobId <- jobAdd spc emptyJob
          jobStatus spc jobId >>= (@?= Just JobPending)
          jobCancel spc jobId
          jobStatus spc jobId >>= (@?= Just (JobDone DoneCancelled)),
        testCase "cancelling non-pending job does not fail" $ do
          spc <- startSPC
          jobId <- jobAdd spc emptyJob
          let missingJobId = succ jobId
          jobStatus spc missingJobId >>= (@?= Nothing)
          jobCancel spc missingJobId
          jobStatus spc missingJobId >>= (@?= Nothing)
      ]
