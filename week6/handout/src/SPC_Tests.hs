module SPC_Tests (tests) where

import Control.Concurrent (threadDelay)
import Data.IORef
import SPC
import Test.Tasty (TestTree, localOption, mkTimeout, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import Control.Monad (replicateM_)

tests :: TestTree
tests =
  localOption (mkTimeout 3000000) $
    testGroup
      "SPC"
      [ testCase "ping should pong" $ startSPC >>= pingSPC >>= (@?= 0),
        testCase "server loop should continue running" $ do
          spc <- startSPC
          replicateM_ 10 $ pingSPC spc >>= (@?= 0)
      ]
