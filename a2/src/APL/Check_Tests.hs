module APL.Check_Tests (tests) where

import APL.AST (Exp (..))
import APL.Check (checkExp)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase, (@?=))

-- Assert that the provided expression should pass the type checker.
testPos :: Exp -> TestTree
testPos e =
  testCase (show e) $
    checkExp e @?= Nothing

-- Assert that the provided expression should fail the type checker.
testNeg :: Exp -> TestTree
testNeg e =
  testCase (show e) $
    case checkExp e of
      Nothing -> assertFailure "expected error"
      Just _ -> pure ()

tests :: TestTree
tests =
  testGroup
    "Checking"
    [
      testPos $ CstInt 2,
      testNeg $ Var "x",
      testPos $ Lambda "x" (Var "x"),
      testNeg $ Lambda "x" (Var "y"),
      testPos $ ForLoop ("p", CstInt 2) ("i", CstInt 5) (Add (Var "p") (Var "i")),
      testNeg $ ForLoop ("p", CstInt 2) ("i", CstInt 5) (Var "a"),
      testPos $ If (CstBool True) (Let "x" (CstInt 1) (Var "x")) (CstInt 0),
      testNeg $ If (CstBool True) (Let "x" (CstInt 1) (CstInt 0)) (Var "x"),
      testPos $ Let "x" (Let "y" (CstBool False) (Var "y")) (CstInt 0),
      testNeg $ Let "x" (Let "y" (CstBool False) (CstInt 0)) (Var "y")
    ]
