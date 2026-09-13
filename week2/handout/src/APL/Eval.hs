module APL.Eval
  ( Val (..),
    eval,
    runEval,
    envEmpty,
    Env,
    Error,
  )
where

import APL.AST (Exp (..), VName)
import Control.Monad (ap, liftM)
import Data.Function ((&))

data Val
  = ValInt Integer
  | ValBool Bool
  | ValFun Env VName Exp
  deriving (Eq, Show)

type Env = [(VName, Val)]

envEmpty :: Env
envEmpty = []

envExtend :: VName -> Val -> Env -> Env
envExtend v val env = (v, val) : env

envLookup :: VName -> Env -> Maybe Val
envLookup v env = lookup v env

type Error = String

newtype EvalM a = EvalM (Either Error a)
  deriving (Show, Eq)

instance Functor EvalM where
  -- fmap _ (EvalM (Left e)) = EvalM $ Left e
  -- fmap f (EvalM (Right a)) = EvalM $ Right (f a)
  fmap = liftM

instance Applicative EvalM where
  pure a = EvalM $ Right a

  -- EvalM (Left e) <*> _ = EvalM $ Left e
  -- _ <*> EvalM (Left e) = EvalM $ Left e
  -- EvalM (Right f) <*> EvalM (Right a) = EvalM $ Right (f a)
  (<*>) = ap

instance Monad EvalM where
  EvalM (Left e) >>= _ = EvalM $ Left e
  EvalM (Right a) >>= f = f a

runEval :: EvalM a -> Either Error a
runEval (EvalM x) = x

failure :: String -> EvalM a
failure e = EvalM $ Left e

evalIntBinOp :: (Integer -> Integer -> EvalM Integer) -> Env -> Exp -> Exp -> EvalM Val
evalIntBinOp f env e1 e2 = do
  x1 <- eval env e1
  x2 <- eval env e2
  case (x1, x2) of
    (ValInt x1', ValInt x2') -> ValInt <$> f x1' x2'
    _ -> failure "Non-integer operand"

evalIntBinOp' :: (Integer -> Integer -> Integer) -> Env -> Exp -> Exp -> EvalM Val
evalIntBinOp' f env e1 e2 =
  evalIntBinOp f' env e1 e2
  where
    f' x y = pure $ f x y

eval :: Env -> Exp -> EvalM Val
eval _ (CstInt x) = pure $ ValInt x
eval _ (CstBool b) = pure $ ValBool b
eval env (Var v) = do
  case envLookup v env of
    Just x -> pure x
    Nothing -> failure $ "Unknown variable: " ++ v
eval env (Add e1 e2) = evalIntBinOp' (+) env e1 e2
eval env (Sub e1 e2) = evalIntBinOp' (-) env e1 e2
eval env (Mul e1 e2) = evalIntBinOp' (*) env e1 e2
eval env (Div e1 e2) = evalIntBinOp checkedDiv env e1 e2
  where
    checkedDiv _ 0 = failure "Division by zero"
    checkedDiv x y = pure $ x `div` y
eval env (Pow e1 e2) = evalIntBinOp checkedPow env e1 e2
  where
    checkedPow x y
      | y < 0 = failure "Negative exponent"
      | otherwise = pure $ x ^ y
eval env (Eql e1 e2) = do
  x1 <- eval env e1
  x2 <- eval env e2
  case (x1, x2) of
    (ValInt x1', ValInt x2') -> pure $ ValBool $ x1' == x2'
    (ValBool x1', ValBool x2') -> pure $ ValBool $ x1' == x2'
    _ -> failure "Invalid operands to equality"
eval env (If cond e1 e2) = do
  result <- eval env cond
  case result of
    ValBool b -> eval env (if b then e1 else e2)
    _ -> failure "Non-boolean conditional."
eval env (Let var e1 e2) = do
  x1 <- eval env e1
  let newEnv = envExtend var x1 env
   in eval newEnv e2
eval env (ForLoop (p, initial) (i, bound) body) = do
  initial' <- eval env initial
  bound' <- eval env bound
  case bound' of
    ValInt boundInt -> evalFor env p i initial' 0 boundInt body
    _ -> failure "Non-integral loop bound"
eval env (Lambda vname body) = pure $ ValFun env vname body
eval env (Apply fexp arg) = do
  fexp' <- eval env fexp
  arg' <- eval env arg
  case fexp' of
    ValFun fenv vname body ->
      let newEnv = envExtend vname arg' fenv
       in eval newEnv body
    _ -> failure "Cannot apply non-functional expression"
eval env (TryCatch body catchBody) = case (eval env body, eval env catchBody) of
  (EvalM (Left _), catchVal) -> catchVal
  (val@(EvalM (Right _)), _) -> val

evalFor :: Env -> VName -> VName -> Val -> Integer -> Integer -> Exp -> EvalM Val
evalFor env p i pval ival bound body
  | bound == ival = pure pval
  | otherwise = do
      bval <- eval newEnv body
      evalFor newEnv p i bval (ival + 1) bound body
  where
    newEnv = env & envExtend p pval & envExtend i (ValInt ival)
