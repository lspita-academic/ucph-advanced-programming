module APL.Eval_Tests (tests) where

import APL.AST
  ( Exp
      ( Add,
        CstBool,
        CstInt,
        Div,
        Eql,
        If,
        Let,
        Mul,
        Pow,
        Sub,
        Var
      ),
  )
import APL.Eval
  ( EvalError
      ( DivisionByZero,
        InvalidOperandsType,
        NegativeExponent,
        NonBooleanCondition,
        UndefinedReference
      ),
    Val (ValBool, ValInt),
    envEmpty,
    envExtend,
    eval,
  )
import Data.Function ((&))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, testCase)

tests :: TestTree
tests =
  testGroup
    "Evaluation"
    [ testGroup
        "Constants evaluation"
        [ testCase "Evaluate constant ints" $
            assertBool "should evaluate to int" $
              Left (ValInt 2) == eval envEmpty (CstInt 2),
          testCase "Evaluate negative constant ints" $
            assertBool "should evaluate to negative int" $
              Left (ValInt (-2)) == eval envEmpty (CstInt (-2)),
          testCase "Evaluate different constant ints" $
            assertBool "should not be equal values" $
              eval envEmpty (CstInt 2) /= eval envEmpty (CstInt 3),
          testCase "Evaluate constant booleans" $
            assertBool "should evaluate to boolean" $
              Left (ValBool True) == eval envEmpty (CstBool True),
          testCase "Evaluate different constant booleans" $
            assertBool "should not be equal values" $
              eval envEmpty (CstBool True) /= eval envEmpty (CstBool False)
        ],
      testGroup
        "Arithmetic operations"
        [ testCase "Evaluate addition" $
            assertBool "should add two ints" $
              Left (ValInt 5) == eval envEmpty (CstInt 2 `Add` CstInt 3),
          testCase "Evaluate subtraction" $
            assertBool "should subtract two ints" $
              Left (ValInt (-1)) == eval envEmpty (CstInt 2 `Sub` CstInt 3),
          testCase "Evaluate multiplication" $
            assertBool "should multiply two ints" $
              Left (ValInt 6) == eval envEmpty (CstInt 2 `Mul` CstInt 3),
          testCase "Evaluate division" $
            assertBool "should divide two ints" $
              Left (ValInt 2) == eval envEmpty (CstInt 6 `Div` CstInt 3),
          testCase "Evaluate exponentiation" $
            assertBool "should exponentiate two ints" $
              Left (ValInt 8) == eval envEmpty (CstInt 2 `Pow` CstInt 3)
        ],
      testGroup
        "Arithmetic operations failure"
        [ testCase "Division by 0" $
            assertBool "should return a division by 0 error" $
              Right DivisionByZero == eval envEmpty (CstInt 2 `Div` CstInt 0),
          testCase "Negative exponent" $
            assertBool "should return a negative exponent error" $
              Right NegativeExponent == eval envEmpty (CstInt 2 `Pow` CstInt (-3)),
          testCase "Addition with invalid operands" $
            assertBool "should return an invalid operands type error" $
              Right InvalidOperandsType == eval envEmpty (CstInt 2 `Add` CstBool True),
          testCase "Subtraction with invalid operands" $
            assertBool "should return an invalid operands type error" $
              Right InvalidOperandsType == eval envEmpty (CstInt 2 `Sub` CstBool True),
          testCase "Multiplication with invalid operands" $
            assertBool "should return an invalid operands type error" $
              Right InvalidOperandsType == eval envEmpty (CstInt 2 `Mul` CstBool True),
          testCase "Division with invalid operands" $
            assertBool "should return an invalid operands type error" $
              Right InvalidOperandsType == eval envEmpty (CstInt 2 `Div` CstBool True),
          testCase "Exponentiation with invalid operands" $
            assertBool "should return an invalid operands type error" $
              Right InvalidOperandsType == eval envEmpty (CstInt 2 `Pow` CstBool True)
        ],
      testGroup
        "Conditionals operations"
        [ testCase "Evaluate integers equality" $
            assertBool "integers should be equal" $
              Left (ValBool True) == eval envEmpty (CstInt 2 `Eql` CstInt 2),
          testCase "Evaluate integers inequality" $
            assertBool "integers should not be equal" $
              Left (ValBool False) == eval envEmpty (CstInt 3 `Eql` CstInt 2),
          testCase "Evaluate booleans equality" $
            assertBool "booleans should be equal" $
              Left (ValBool True) == eval envEmpty (CstBool True `Eql` CstBool True),
          testCase "Evaluate booleans inequality" $
            assertBool "booleans should not be equal" $
              Left (ValBool False) == eval envEmpty (CstBool True `Eql` CstBool False),
          testCase "Evaluate true if statement" $
            assertBool "should evaluate first expression" $
              Left (ValInt 0) == eval envEmpty (If (CstBool True) (CstInt 0) (CstInt 1)),
          testCase "Evaluate false if statement" $
            assertBool "should evaluate second expression" $
              Left (ValInt 1) == eval envEmpty (If (CstBool False) (CstInt 0) (CstInt 1)),
          testCase "Test lazy evaluation of expressions in if statement" $
            assertBool "should not evaluate the error" $
              Left (ValInt 0) == eval envEmpty (If (CstBool True) (CstInt 0) (CstInt 1 `Div` CstInt 0))
        ],
      testGroup
        "Conditional operations failure"
        [ testCase "If statement with non boolean condition" $
            assertBool "should return a non boolean condition error" $
              Right NonBooleanCondition == eval envEmpty (If (CstInt 0) (CstInt 0) (CstInt 1)),
          testCase "Equality between different types" $
            assertBool "should return an invalid operands type error" $
              Right InvalidOperandsType == eval envEmpty (CstInt 2 `Eql` CstBool True)
        ],
      testGroup
        "Variables"
        [ testCase "Read a preexisting variable" $
            assertBool "should return the variable value" $
              let env = envEmpty & envExtend "x" (ValInt 0)
               in Left (ValInt 0) == eval env (Var "x"),
          testCase "Define and read a variable" $
            assertBool "should define and return the variable value" $
              Left (ValInt 0) == eval envEmpty (Let "x" (CstInt 0) (Var "x")),
          testCase "Override an existing variable" $
            assertBool "should return the second value assigned" $
              Left (ValInt 2)
                == eval
                  envEmpty
                  ( Let
                      "x"
                      (CstInt 1)
                      ( Let
                          "x"
                          (CstInt 2)
                          (Var "x")
                      )
                  ),
          testCase "Use variables in expressions" $
            assertBool "should use variables in expressions" $
              Left (ValInt 3)
                == eval
                  envEmpty
                  ( Let
                      "x"
                      (CstInt 1)
                      ( Let
                          "y"
                          (CstInt 2)
                          (Var "x" `Add` Var "y")
                      )
                  )
        ],
      testGroup
        "Variables failure"
        [ testCase "Read an undefined variable" $
            assertBool "should return an undefined reference error" $
              Right (UndefinedReference "x") == eval envEmpty (Var "x"),
          testCase "Fail to evaluate the variable expression" $
            assertBool "should fail to declare the variable" $
              Right DivisionByZero == eval envEmpty (Let "x" (CstInt 1 `Div` CstInt 0) (Var "x")),
          testCase "Lazyly evaluate the body expression" $
            assertBool "should not evaluate the body expression" $
              Right DivisionByZero == eval envEmpty (Let "x" (CstInt 1 `Div` CstInt 0) (CstInt 2 `Pow` CstInt (-3)))
        ]
    ]
