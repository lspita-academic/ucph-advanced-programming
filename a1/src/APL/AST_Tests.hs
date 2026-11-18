module APL.AST_Tests (tests) where

import APL.AST (Exp (..), printExp)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

tests :: TestTree
tests =
  testGroup
    "Prettyprinting"
    [ testCase "CstInt" $
        printExp (CstInt (-5))
          @?= "-5",
      --
      testCase "CstBool" $
        printExp (CstBool False)
          @?= "False",
      --
      testCase "Add" $
        printExp (Add (CstInt 2) (CstInt 5))
          @?= "(2 + 5)",
      --
      testCase "Sub" $
        printExp (Sub (CstInt 2) (CstInt 5))
          @?= "(2 - 5)",
      --
      testCase "Mul" $
        printExp (Mul (CstInt 2) (CstInt 5))
          @?= "(2 * 5)",
      --
      testCase "Div" $
        printExp (Div (CstInt 2) (CstInt 5))
          @?= "(2 / 5)",
      --
      testCase "Pow" $
        printExp (Pow (CstInt 2) (CstInt 5))
          @?= "(2 ** 5)",
      --
      testCase "Eql" $
        printExp (Eql (CstInt 2) (CstInt 5))
          @?= "(2 == 5)",
      --
      testCase "If" $
        printExp (If (Eql (Add (CstInt 1) (CstInt 1)) (CstInt 2)) (CstBool True) (CstBool False))
          @?= "(if ((1 + 1) == 2) then True else False)",
      --
      testCase "Var" $
        printExp (Var "myVar")
          @?= "myVar",
      --
      testCase "Let" $
        printExp (Let "myVar" (CstInt 10) (CstBool True))
          @?= "(let myVar = 10 in True)",
      --
      testCase "ForLoop" $
        printExp (ForLoop ("acc", CstInt 0) ("iter", CstInt 10) (Add (Var "acc") (Var "iter")))
          @?= "(loop acc = 0 for iter < 10 do (acc + iter))",
      --
      testCase "Lambda" $
        printExp (Lambda "x" (Mul (Var "x") (CstInt 2)))
          @?= "(\\x -> (x * 2))",
      --
      testCase "Apply" $
        printExp (Apply (Lambda "x" (Mul (Var "x") (CstInt 2))) (CstInt 5))
          @?= "((\\x -> (x * 2)) 5)",
      --
      testCase "TryCatch" $
        printExp (TryCatch (Div (CstInt 1) (CstInt 0)) (CstInt 42))
          @?= "(try (1 / 0) catch 42)"
    ]
