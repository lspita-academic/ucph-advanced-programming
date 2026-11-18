module APL.Interp_Tests (tests) where

import APL.AST (Exp (..))
import APL.Eval (eval)
import APL.InterpIO (runEvalIO)
import APL.InterpPure (runEval)
import APL.Monad
import APL.Util (captureIO)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, assertFailure, testCase, (@?=))

eval' :: Exp -> ([String], Either Error Val)
eval' = runEval . eval

evalIO' :: Exp -> IO (Either Error Val)
evalIO' = runEvalIO . eval

tests :: TestTree
tests = testGroup "Free monad interpreters" [pureTests, ioTests]

itFails :: ([String], Either Error Val) -> [String] -> Assertion
itFails val logs = case val of
  (_, Right _) -> assertFailure "Test did not fail"
  (l, Left _) -> l @?= logs

pureTests :: TestTree
pureTests =
  testGroup
    "Pure interpreter"
    [ testCase "localEnv" $
        runEval
          ( localEnv (const [("x", ValInt 1)]) $
              askEnv
          )
          @?= ([], Right [("x", ValInt 1)]),
      --
      testCase "Let" $
        eval' (Let "x" (Add (CstInt 2) (CstInt 3)) (Var "x"))
          @?= ([], Right (ValInt 5)),
      --
      testCase "Let (shadowing)" $
        eval'
          ( Let
              "x"
              (Add (CstInt 2) (CstInt 3))
              (Let "x" (CstBool True) (Var "x"))
          )
          @?= ([], Right (ValBool True)),
      --
      testCase "Print" $
        runEval (evalPrint "test")
          @?= (["test"], Right ()),
      --
      testCase "Error" $
        runEval
          ( do
              _ <- failure "Oh no!"
              evalPrint "test"
          )
          @?= ([], Left "Oh no!"),
      --
      testCase "Div0" $
        eval' (Div (CstInt 7) (CstInt 0))
          @?= ([], Left "Division by zero"),
      --
      testCase "TryCatch - try pass" $
        eval' (TryCatch (CstInt 3) (Div (CstInt 1) (CstInt 0)))
          @?= ([], Right (ValInt 3)),
      --
      testCase "TryCatch - try fail" $
        eval' (TryCatch (Div (CstInt 1) (CstInt 0)) (CstInt 1))
          @?= ([], Right (ValInt 1)),
      --
      testCase "TryCatch - catch fail" $
        eval' (TryCatch (Div (CstInt 1) (CstInt 0)) (Div (CstInt 1) (CstInt 0)))
          @?= ([], Left "Division by zero"),
      --
      testCase "TryCatch - try fail keep prints" $
        eval' (TryCatch (Print "m1" (CstInt 3)) (Div (CstInt 1) (CstInt 0)))
          @?= (["m1: 3"], Right (ValInt 3)),
      --
      testCase "TryCatch - KVStore effects visible in catch" $
        eval' (TryCatch (Let "_" (KvPut (CstInt 0) (CstInt 42)) (Div (CstInt 1) (CstInt 0))) (KvGet (CstInt 0)))
          @?= ([], Right (ValInt 42)),
      --      
      testCase "KVStore - write read" $
        eval' (Let "_" (KvPut (CstInt 0) (CstInt 1)) (KvGet (CstInt 0)))
          @?= ([], Right (ValInt 1)),
      --
      testCase "KVStore - invalid key" $
        itFails (eval' (Let "_" (KvPut (CstInt 0) (CstInt 1)) (KvGet (CstInt 1)))) [],
      --
      testCase "Transaction - pass" $
        eval' (Let "_" (Transaction (KvPut (CstInt 0) (CstInt 1))) (KvGet (CstInt 0)))
          @?= ([], Right (ValInt 1)),
      --
      testCase "Transaction - rollback" $
        eval' (TryCatch (Transaction (Let "_" (KvPut (CstInt 0) (CstBool False)) (Var "die"))) (KvGet (CstInt 0)))
          @?= ([], Left "Invalid key: ValInt 0"),
      --
      testCase "Transaction - rollback error propagation" $
        eval' (Transaction (Let "_" (KvPut (CstInt 0) (CstBool False)) (Var "die")))
          @?= ([], Left "Unknown variable: die"),
      --
      testCase "Transaction - rollback keep prints" $
        runEval (evalPrint "wooo" >> transaction (evalPrint "weee" >> failure "oh shit"))
          @?= (["wooo", "weee"], Left "oh shit"),
      --
      testCase "Transaction - nesting rollback" $
        eval'
          ( Let
              "_"
              ( Transaction
                  ( Let
                      "_"
                      (KvPut (CstInt 0) (CstInt 1))
                      ( TryCatch
                          (Transaction (Let "_" (KvPut (CstInt 0) (CstBool False)) (Var "die")))
                          (CstBool True)
                      )
                  )
              )
              (KvGet (CstInt 0))
          )
          @?= ([], Right (ValInt 1)),
      --
      testCase "Transaction - nesting double rollback" $
        eval'
          ( Let
              "_"
              ( TryCatch
                  ( Transaction
                      (Transaction (Let "_" (KvPut (CstInt 0) (CstBool False)) (Var "die")))
                  )
                  (CstBool True)
              )
              (KvGet (CstInt 0))
          )
          @?= ([], Left "Invalid key: ValInt 0"),
      --
      testCase "Break - ForLoop immediate" $
       eval' (ForLoop ("p", CstInt 0) ("i", CstInt 100) $ Let "_" (Break (CstBool True)) (Var "i"))
        @?= ([], Right (ValBool True)),
      --
      testCase "Break - ForLoop 10th iter" $
         eval' (ForLoop ("p", CstInt 0) ("i", CstInt 100) $ Let "_" (If (Eql (Var "i") (CstInt 9)) (Break (CstBool True)) (CstBool False)) (Var "i"))
        @?= ([], Right (ValBool True)),
      --
      testCase "Break - outside ForLoop" $
        eval' (Break (CstBool True))
        @?= ([], Left "Break outside loop"),
      --
      testCase "Break - only exits innermost loop" $
       eval'
              ( ForLoop ("p", CstInt 0) ("i", CstInt 3) $
                  Add (Var "p") $
                    ForLoop ("q", CstInt 0) ("j", CstInt 10) (Break (CstInt 1))
              )
        @?= ([], Right (ValInt 3)),
      --
      testCase "Break - not caught by TryCatch" $
       eval' (ForLoop ("p", CstInt 0) ("i", CstInt 10) $ TryCatch (Break (CstInt 7)) (Print "no" (CstInt 99)))
        @?= ([], Right (ValInt 7)),
      --
      testCase "Break - no rollback in Transaction" $
       eval'
          ( Let
              "_"
              ( ForLoop ("p", CstInt 0) ("i", CstInt 10) $
                  Transaction (Let "_" (KvPut (CstInt 0) (CstInt 5)) (Break (CstInt 1)))
              )
              (KvGet (CstInt 0))
          )
        @?= ([], Right (ValInt 5)),
      --
      testCase "Break - keeps prints" $
       eval'
          ( ForLoop ("p", CstInt 0) ("i", CstInt 5) $
              Let "_" (Print "hi" (CstInt 0)) (Break (CstInt 1))
          )
        @?= (["hi: 0"], Right (ValInt 1)),
      --
      testCase "Break - doesn't affect KVStore" $
       eval'
        ( Let
            "_"
            ( ForLoop ("p", CstInt 0) ("i", CstInt 10) $
                Let "_" (KvPut (CstInt 0) (CstInt 5)) (Break (CstInt 1))
            )
            (KvGet (CstInt 0))
        )
        @?= ([], Right (ValInt 5)),
      --
      testCase "Break - looping without break" $
        runEval (looping (pure (ValInt 4)))
          @?= ([], Right (ValInt 4))
    ]

ioTests :: TestTree
ioTests =
  testGroup
    "IO interpreter"
    [ testCase "print" $ do
        let s1 = "Lalalalala"
            s2 = "Weeeeeeeee"
        (out, res) <-
          captureIO [] $
            runEvalIO $ do
              evalPrint s1
              evalPrint s2
        (out, res) @?= ([s1, s2], Right ()),
      --
      -- NOTE: This test will give a runtime error unless you replace the
      -- version of `eval` in `APL.Eval` with a complete version that supports
      -- `Print`-expressions. Uncomment at your own risk.
      testCase "print 2" $ do
         (out, res) <-
           captureIO [] $
             evalIO' $
               Print "This is also 1" $
                 Print "This is 1" $
                   CstInt 1
         (out, res) @?= (["This is 1: 1", "This is also 1: 1"], Right $ ValInt 1),
      --
      testCase "TryCatch - try pass" $ do
        (out, res) <- captureIO [] $ evalIO' (TryCatch (CstInt 3) (Div (CstInt 1) (CstInt 0)))
        (out, res)  @?= ([], Right (ValInt 3)),
      --
      testCase "TryCatch - try fail" $ do
        (out, res) <- captureIO [] $ evalIO' (TryCatch (Div (CstInt 1) (CstInt 0)) (Print "lol" (CstInt 1)))
        (out, res) @?= (["lol: 1"], Right (ValInt 1)),
      --
      testCase "TryCatch - catch fail" $ do
        (out, res) <- captureIO [] $ evalIO' (TryCatch (Div (CstInt 1) (CstInt 0)) (Div (CstInt 1) (CstInt 0)))
        (out, res) @?= ([], Left "Division by zero"),
      --
      testCase "TryCatch - try fail keep prints" $ do
        (out, res) <- captureIO [] $ evalIO' (TryCatch (Print "m1" (CstInt 3)) (Div (CstInt 1) (CstInt 0)))
        (out, res)  @?= (["m1: 3"], Right (ValInt 3)),
      --
      testCase "TryCatch - KVStore effects visible in catch" $ do
        (out, res) <- captureIO [] $ evalIO' (TryCatch (Let "_" (KvPut (CstInt 0) (CstInt 42)) (Div (CstInt 1) (CstInt 0))) (KvGet (CstInt 0)))
        (out, res) @?= ([], Right (ValInt 42)),
      --
      testCase "KVStore - write read" $ do
        (out, res) <- captureIO [] $ evalIO' (Let "_" (KvPut (CstInt 0) (CstInt 1)) (KvGet (CstInt 0)))
        (out, res) @?= ([], Right (ValInt 1)),
      --
      testCase "KVStore - missing key (ValInt)" $ do
        (out, res) <- captureIO ["ValInt 42"] $ evalIO' (KvGet (CstInt 0))
        (out, res) @?= (["Invalid key: ValInt 0. Enter a replacement: "], Right (ValInt 42)),
      --
      testCase "KVStore - missing key (ValBool)" $ do
        (out, res) <- captureIO ["ValBool True"] $ evalIO' (KvGet (CstInt 0))
        (out, res) @?= (["Invalid key: ValInt 0. Enter a replacement: "], Right (ValBool True)),
      --
      testCase "KVStore - missing key (invalid input)" $ do
        (out, res) <- captureIO ["lol"] $ evalIO' (KvGet (CstInt 0))
        itFails (out, res) ["Invalid key: ValInt 0. Enter a replacement: "],
      --
      testCase "KVStore - missing key (impermanence)" $ do
        (out, res) <- captureIO ["ValInt 42", "ValInt 1337"] $ evalIO' (Let "_" (KvGet (CstInt 0)) (KvGet (CstInt 0)))
        (out, res) @?= (["Invalid key: ValInt 0. Enter a replacement: Invalid key: ValInt 0. Enter a replacement: "], Right (ValInt 1337)),
      --
      testCase "Transaction - pass" $ do
        (out, res) <- captureIO [] $ evalIO' (Let "_" (Transaction (KvPut (CstInt 0) (CstInt 1))) (KvGet (CstInt 0)))
        (out, res) @?= ([], Right (ValInt 1)),
      --
      testCase "Transaction - rollback" $ do
        (out, res) <- captureIO ["lol"] $ evalIO' (TryCatch (Transaction (Let "_" (KvPut (CstInt 0) (CstBool False)) (Var "die"))) (KvGet (CstInt 0)))      
        itFails (out, res) ["Invalid key: ValInt 0. Enter a replacement: "],
      --
      testCase "Transaction - rollback error propagation" $ do
        (out, res) <- captureIO [] $ evalIO' (Transaction (Let "_" (KvPut (CstInt 0) (CstBool False)) (Var "die")))
        (out, res) @?= ([], Left "Unknown variable: die"),
      --
      testCase "Transaction - rollback keep prints" $ do
        (out, res) <- captureIO [] $ runEvalIO (evalPrint "wooo" >> transaction (evalPrint "weee" >> failure "oh shit"))
        (out, res) @?= (["wooo", "weee"], Left "oh shit"),
      --
      testCase "Transaction - nesting rollback" $ do
        (out, res) <-
          captureIO [] $
            evalIO'
              ( Let
                  "_"
                  ( Transaction
                      ( Let
                          "_"
                          (KvPut (CstInt 0) (CstInt 1))
                          ( TryCatch
                              (Transaction (Let "_" (KvPut (CstInt 0) (CstBool False)) (Var "die")))
                              (CstBool True)
                          )
                      )
                  )
                  (KvGet (CstInt 0))
              )
        (out, res) @?= ([], Right (ValInt 1)),
      --
      testCase "Transaction - nesting double rollback" $ do
        (out, res) <-
          captureIO ["lol"] $
            evalIO'
              ( Let
                  "_"
                  ( TryCatch
                      ( Transaction
                          (Transaction (Let "_" (KvPut (CstInt 0) (CstBool False)) (Var "die")))
                      )
                      (CstBool True)
                  )
                  (KvGet (CstInt 0))
              )
        itFails (out, res) ["Invalid key: ValInt 0. Enter a replacement: "],
      --
      testCase "Break - ForLoop immediate" $ do
        (out, res) <-
          captureIO [] $
            evalIO' (ForLoop ("p", CstInt 0) ("i", CstInt 100) $ Let "_" (Break (CstBool True)) (Var "i"))
        (out, res) @?= ([], Right (ValBool True)),
      --
      testCase "Break - ForLoop 10th iter" $ do
        (out, res) <-
          captureIO [] $
            evalIO' (ForLoop ("p", CstInt 0) ("i", CstInt 100) $ Let "_" (If (Eql (Var "i") (CstInt 9)) (Break (CstBool True)) (CstBool False)) (Var "i"))
        (out, res) @?= ([], Right (ValBool True)),
      --
      testCase "Break - outside ForLoop" $ do
        (out, res) <- captureIO [] $ evalIO' (Break (CstBool True))
        (out, res) @?= ([], Left "Break outside loop"),
      --
      testCase "Break - only exits innermost loop" $ do
        (out, res) <-
          captureIO [] $
            evalIO'
              ( ForLoop ("p", CstInt 0) ("i", CstInt 3) $
                  Add (Var "p") $
                    ForLoop ("q", CstInt 0) ("j", CstInt 10) (Break (CstInt 1))
              )
        (out, res) @?= ([], Right (ValInt 3)),
      --
      testCase "Break - not caught by TryCatch" $ do
        (out, res) <-
          captureIO [] $
            evalIO' (ForLoop ("p", CstInt 0) ("i", CstInt 10) $ TryCatch (Break (CstInt 7)) (Print "no" (CstInt 99)))
        (out, res) @?= ([], Right (ValInt 7)),
      --
      testCase "Break - no rollback in Transaction" $ do
        (out, res) <-
          captureIO [] $
            evalIO'
              ( Let
                  "_"
                  ( ForLoop ("p", CstInt 0) ("i", CstInt 10) $
                      Transaction (Let "_" (KvPut (CstInt 0) (CstInt 5)) (Break (CstInt 1)))
                  )
                  (KvGet (CstInt 0))
              )
        (out, res) @?= ([], Right (ValInt 5)),
      --
      testCase "Break - keeps prints" $ do
        (out, res) <-
          captureIO [] $
            evalIO'
              ( ForLoop ("p", CstInt 0) ("i", CstInt 5) $
                  Let "_" (Print "hi" (CstInt 0)) (Break (CstInt 1))
              )
        (out, res) @?= (["hi: 0"], Right (ValInt 1)),
      --
      testCase "Break - doesn't affect KVStore" $ do
        (out, res) <-
          captureIO [] $
            evalIO'
              ( Let
                  "_"
                  ( ForLoop ("p", CstInt 0) ("i", CstInt 10) $
                      Let "_" (KvPut (CstInt 0) (CstInt 5)) (Break (CstInt 1))
                  )
                  (KvGet (CstInt 0))
              )
        (out, res) @?= ([], Right (ValInt 5)),
      --
      testCase "Break - looping without break" $ do
        (out, res) <-
          captureIO [] $
            runEvalIO (looping (pure (ValInt 4)))
        (out, res) @?= ([], Right (ValInt 4))
    ]
