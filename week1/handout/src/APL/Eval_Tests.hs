module APL.Eval_Tests (tests) where

import APL.AST (Exp (CstInt))
import APL.Eval (Val (ValInt), eval)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, testCase)

tests :: TestTree
tests =
  testGroup
    "Evaluation"
    [ testCase "Evaluate constant ints" $ assertBool "should evaluate to int" $ ValInt 2 == eval (CstInt 2),
      testCase "Evaluate negative constant ints" $ assertBool "should evaluate to negative int" $ ValInt (-2) == eval (CstInt (-2)),
      testCase "Evaluate different constant ints" $ assertBool "should not be equal values" $ eval (CstInt 2) /= eval (CstInt 3)
    ]
