module APL.Eval_Tests (tests) where

import APL.AST (Exp (Add, CstInt, Div, Mul, Pow, Sub))
import APL.Eval (Val (ValInt), eval)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, testCase)

tests :: TestTree
tests =
  testGroup
    "Evaluation"
    [ testGroup
        "Constants evaluation"
        [ testCase "Evaluate constant ints" $ assertBool "should evaluate to int" $ Left (ValInt 2) == eval (CstInt 2),
          testCase "Evaluate negative constant ints" $ assertBool "should evaluate to negative int" $ Left (ValInt (-2)) == eval (CstInt (-2)),
          testCase "Evaluate different constant ints" $ assertBool "should not be equal values" $ eval (CstInt 2) /= eval (CstInt 3)
        ],
      testGroup
        "Basic operations"
        [ testCase "Evaluate addition" $ assertBool "should add two ints" $ Left (ValInt 5) == eval (CstInt 2 `Add` CstInt 3),
          testCase "Evaluate subtraction" $ assertBool "should subtract two ints" $ Left (ValInt (-1)) == eval (CstInt 2 `Sub` CstInt 3),
          testCase "Evaluate multiplication" $ assertBool "should multiply two ints" $ Left (ValInt 6) == eval (CstInt 2 `Mul` CstInt 3),
          testCase "Evaluate division" $ assertBool "should divide two ints" $ Left (ValInt 2) == eval (CstInt 6 `Div` CstInt 3),
          testCase "Evaluate exponentiation" $ assertBool "should exponentiate two ints" $ Left (ValInt 8) == eval (CstInt 2 `Pow` CstInt 3)
        ]
    ]
