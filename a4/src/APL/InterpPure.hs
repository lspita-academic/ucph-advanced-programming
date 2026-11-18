module APL.InterpPure (runEval) where

import APL.Monad

runEval :: EvalM a -> ([String], Either Error a)
runEval m =
  let (p, _, res) = runEval' envEmpty stateInitial m
   in (p, either (Left . interruptionToError) Right res)
  where
    prependLogs p (p', s, res) = (p ++ p', s, res)

    runEval' :: Env -> State -> EvalM a -> ([String], State, Either EvalInterruption a)
    runEval' _ s (Pure x) = ([], s, pure x)
    runEval' r s (Free (ReadOp k)) = runEval' r s $ k r
    runEval' r s (Free (PrintOp p m1)) = prependLogs [p] $ runEval' r s m1
    runEval' _ s (Free (ErrorOp e)) = ([], s, Left (Fail e))
    runEval' r s (Free (TryCatchOp m1 m2 k)) =
      case runEval' r s m1 of
        (p1, s1, Right v) -> prependLogs p1 $ runEval' r s1 (k v)
        (p1, s1, Left (Stop v)) -> (p1, s1, Left (Stop v))
        (p1, s1, Left (Fail _)) -> prependLogs p1 $ case runEval' r s1 m2 of
          (p2, s2, Right v) -> prependLogs p2 $ runEval' r s2 (k v)
          (p2, s2, Left e) -> (p2, s2, Left e)
    runEval' r s (Free (KvGetOp key k)) = case lookup key s of
      Nothing -> ([], s, Left (Fail ("Invalid key: " ++ show key)))
      Just val -> runEval' r s (k val)
    runEval' r s (Free (KvPutOp key val k)) =
      let keep = filter (\(key', _) -> key' /= key) s
          s1 = (key, val) : keep
       in runEval' r s1 k
    runEval' r s (Free (TransactionOp m1 k)) = case runEval' r s m1 of
      (p1, s1, Right v) -> prependLogs p1 $ runEval' r s1 (k v)
      (p1, s1, Left (Stop v)) -> (p1, s1, Left (Stop v)) -- break doesn't mean failure, state is committed
      (p1, _, Left (Fail e)) -> (p1, s, Left (Fail e))
    runEval' _ s (Free (BreakOp v)) = ([], s, Left (Stop v))
    runEval' r s (Free (LoopingOp m1 k)) = case runEval' r s m1 of
      (p1, s1, Left (Fail e)) -> (p1, s1, Left (Fail e))
      (p1, s1, Left (Stop v)) -> prependLogs p1 $ runEval' r s1 (k v)
      (p1, s1, Right v) -> prependLogs p1 $ runEval' r s1 (k v)
