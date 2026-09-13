module APL.Eval_Tests (tests) where

import APL.AST (Exp (..))
import APL.Eval (Env, Error, Val (..), envEmpty, eval, runEval)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

-- -- Consider this example when you have added the necessary constructors.
-- -- The Y combinator in a form suitable for strict evaluation.
yComb :: Exp
yComb =
  Lambda "f" $
    Apply
      (Lambda "g" (Apply (Var "g") (Var "g")))
      ( Lambda
          "g"
          ( Apply
              (Var "f")
              (Lambda "a" (Apply (Apply (Var "g") (Var "g")) (Var "a")))
          )
      )

fact :: Exp
fact =
  Apply yComb $
    Lambda "rec" $
      Lambda "n" $
        If
          (Eql (Var "n") (CstInt 0))
          (CstInt 1)
          (Mul (Var "n") (Apply (Var "rec") (Sub (Var "n") (CstInt 1))))

eval' :: Env -> Exp -> Either Error Val
eval' env e = runEval $ eval env e

tests :: TestTree
tests =
  testGroup
    "Evaluation"
    [ testCase "Add" $
        eval' envEmpty (Add (CstInt 2) (CstInt 5))
          @?= Right (ValInt 7),
      --
      testCase "Add (wrong type)" $
        eval' envEmpty (Add (CstInt 2) (CstBool True))
          @?= Left "Non-integer operand",
      --
      testCase "Sub" $
        eval' envEmpty (Sub (CstInt 2) (CstInt 5))
          @?= Right (ValInt (-3)),
      --
      testCase "Div" $
        eval' envEmpty (Div (CstInt 7) (CstInt 3))
          @?= Right (ValInt 2),
      --
      testCase "Div0" $
        eval' envEmpty (Div (CstInt 7) (CstInt 0))
          @?= Left "Division by zero",
      --
      testCase "Pow" $
        eval' envEmpty (Pow (CstInt 2) (CstInt 3))
          @?= Right (ValInt 8),
      --
      testCase "Pow0" $
        eval' envEmpty (Pow (CstInt 2) (CstInt 0))
          @?= Right (ValInt 1),
      --
      testCase "Pow negative" $
        eval' envEmpty (Pow (CstInt 2) (CstInt (-1)))
          @?= Left "Negative exponent",
      --
      testCase "Eql (false)" $
        eval' envEmpty (Eql (CstInt 2) (CstInt 3))
          @?= Right (ValBool False),
      --
      testCase "Eql (true)" $
        eval' envEmpty (Eql (CstInt 2) (CstInt 2))
          @?= Right (ValBool True),
      --
      testCase "If" $
        eval' envEmpty (If (CstBool True) (CstInt 2) (Div (CstInt 7) (CstInt 0)))
          @?= Right (ValInt 2),
      --
      testCase "Let" $
        eval' envEmpty (Let "x" (Add (CstInt 2) (CstInt 3)) (Var "x"))
          @?= Right (ValInt 5),
      --
      testCase "Let (shadowing)" $
        eval'
          envEmpty
          ( Let
              "x"
              (Add (CstInt 2) (CstInt 3))
              (Let "x" (CstBool True) (Var "x"))
          )
          @?= Right (ValBool True),
      --
      testCase "ForLoop (p)" $
        eval' envEmpty (ForLoop ("p", CstInt 3) ("i", CstInt 5) (Add (Var "p") (CstInt 1)))
          @?= Right (ValInt 8),
      --
      testCase "ForLoop (i)" $
        eval' envEmpty (ForLoop ("p", CstInt 0) ("i", CstInt 5) (Add (Var "p") (Var "i")))
          @?= Right (ValInt 10),
      --
      testCase "ForLoop (initial)" $
        eval' envEmpty (ForLoop ("p", CstBool True) ("i", CstInt 0) (CstBool False))
          @?= Right (ValBool True),
      --
      testCase "ForLoop (let i)" $
        eval' envEmpty (ForLoop ("p", CstInt 0) ("i", CstInt 5) (Let "i" (CstInt 10) (Add (Var "p") (CstInt 1))))
          @?= Right (ValInt 5),
      --
      testCase "ForLoop (non-integral loop bound)" $
        eval' envEmpty (ForLoop ("p", CstBool True) ("i", CstBool True) (CstBool False))
          @?= Left "Non-integral loop bound",
      --
      testCase "Lambda & Apply" $
        eval' envEmpty (Apply (Let "y" (CstInt 2) (Lambda "x" (Add (Var "x") (Var "y")))) (CstInt 3)) @?= Right (ValInt 5),
      --
      testCase "Lambda & Apply (fact)" $
        eval' envEmpty (Apply fact (CstInt 6)) @?= Right (ValInt 720),
      --
      testCase "Apply evaluation order" $
        eval' envEmpty (Apply (Var "x") (Div (CstInt 4) (CstInt 0)))
          @?= Left "Unknown variable: x",
      --
      testCase "TryCatch (pass)" $
        eval' envEmpty (TryCatch (CstBool True) (CstBool False)) @?= Right (ValBool True),
      --
      testCase "TryCatch (eval order)" $
        eval' envEmpty (TryCatch (CstInt 42) (Div (CstInt 1) (CstInt 0))) @?= Right (ValInt 42), --
        --
      testCase "TryCatch (fail)" $
        eval' envEmpty (TryCatch (Div (CstInt 1) (CstInt 0)) (CstInt 42)) @?= Right (ValInt 42)
    ]
