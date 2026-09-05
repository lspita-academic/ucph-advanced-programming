module APL.Eval_Tests (tests) where

import APL.AST (Exp (Add, CstBool, CstInt, Div, Eql, Mul, Pow, Sub))
import APL.Eval (EvalError (DivisionByZero, NegativeExponent), Val (ValBool, ValInt), eval)
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
          testCase "Evaluate different constant ints" $ assertBool "should not be equal values" $ eval (CstInt 2) /= eval (CstInt 3),
          testCase "Evaluate constant booleans" $ assertBool "should evaluate to boolean" $ Left (ValBool True) == eval (CstBool True),
          testCase "Evaluate different constant booleans" $ assertBool "should not be equal values" $ eval (CstBool True) /= eval (CstBool False)
        ],
      testGroup
        "Arithmetic operations"
        [ testCase "Evaluate addition" $ assertBool "should add two ints" $ Left (ValInt 5) == eval (CstInt 2 `Add` CstInt 3),
          testCase "Evaluate subtraction" $ assertBool "should subtract two ints" $ Left (ValInt (-1)) == eval (CstInt 2 `Sub` CstInt 3),
          testCase "Evaluate multiplication" $ assertBool "should multiply two ints" $ Left (ValInt 6) == eval (CstInt 2 `Mul` CstInt 3),
          testCase "Evaluate division" $ assertBool "should divide two ints" $ Left (ValInt 2) == eval (CstInt 6 `Div` CstInt 3),
          testCase "Evaluate exponentiation" $ assertBool "should exponentiate two ints" $ Left (ValInt 8) == eval (CstInt 2 `Pow` CstInt 3)
        ],
      testGroup
        "Arithmetic operations failure"
        [ testCase "Division by 0" $ assertBool "should return a division by 0 error" $ Right DivisionByZero == eval (CstInt 2 `Div` CstInt 0),
          testCase "Negative exponent" $ assertBool "should return a negative exponent error " $ Right NegativeExponent == eval (CstInt 2 `Pow` CstInt (-3))
        ],
      testGroup
        "Conditionals operations"
        [ testCase "Evaluate integers equality" $ assertBool "integers should be equal" $ Left (ValBool True) == eval (CstInt 2 `Eql` CstInt 2),
          testCase "Evaluate integers inequality" $ assertBool "integers should not be equal" $ Left (ValBool False) == eval (CstInt 3 `Eql` CstInt 2),
          testCase "Evaluate booleans equality" $ assertBool "booleans should be equal" $ Left (ValBool True) == eval (CstBool True `Eql` CstBool True),
          testCase "Evaluate booleans inequality" $ assertBool "booleans should not be equal" $ Left (ValBool False) == eval (CstBool True `Eql` CstBool False)
        ]
    ]
