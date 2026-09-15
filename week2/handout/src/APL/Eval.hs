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

newtype EvalM a = EvalM (Env -> Either Error a)

instance Functor EvalM where
  -- fmap _ (EvalM (Left e)) = EvalM $ Left e
  -- fmap f (EvalM (Right a)) = EvalM $ Right (f a)
  fmap = liftM

instance Applicative EvalM where
  pure a = EvalM $ const $ Right a

  -- EvalM (Left e) <*> _ = EvalM $ Left e
  -- _ <*> EvalM (Left e) = EvalM $ Left e
  -- EvalM (Right f) <*> EvalM (Right a) = EvalM $ Right (f a)
  (<*>) = ap

instance Monad EvalM where
  EvalM f >>= fnext = EvalM $ \e -> case f e of
    Left err -> Left err
    Right a ->
      let EvalM f' = fnext a
       in f' e

runEval :: EvalM a -> Env -> Either Error a
runEval (EvalM f) env = f env

failure :: String -> EvalM a
failure e = EvalM $ \_ -> Left e

askEnv :: EvalM Env
askEnv = EvalM $ \env -> Right env

localEnv :: (Env -> Env) -> EvalM a -> EvalM a
localEnv f (EvalM m) = EvalM $ \env -> m (f env)

catch :: EvalM a -> EvalM a -> EvalM a
catch (EvalM m1) (EvalM m2) = EvalM $ \env ->
  let res = m1 env
   in case res of
        Left _ -> m2 env
        Right _ -> res

evalIntBinOp :: (Integer -> Integer -> EvalM Integer) -> Exp -> Exp -> EvalM Val
evalIntBinOp f e1 e2 = do
  x1 <- eval e1
  x2 <- eval e2
  case (x1, x2) of
    (ValInt x1', ValInt x2') -> ValInt <$> f x1' x2'
    _ -> failure "Non-integer operand"

evalIntBinOp' :: (Integer -> Integer -> Integer) -> Exp -> Exp -> EvalM Val
evalIntBinOp' f e1 e2 =
  evalIntBinOp f' e1 e2
  where
    f' x y = pure $ f x y

eval :: Exp -> EvalM Val
eval (CstInt x) = pure $ ValInt x
eval (CstBool b) = pure $ ValBool b
eval (Var v) = do
  env <- askEnv
  case envLookup v env of
    Just x -> pure x
    Nothing -> failure $ "Unknown variable: " ++ v
eval (Add e1 e2) = evalIntBinOp' (+) e1 e2
eval (Sub e1 e2) = evalIntBinOp' (-) e1 e2
eval (Mul e1 e2) = evalIntBinOp' (*) e1 e2
eval (Div e1 e2) = evalIntBinOp checkedDiv e1 e2
  where
    checkedDiv _ 0 = failure "Division by zero"
    checkedDiv x y = pure $ x `div` y
eval (Pow e1 e2) = evalIntBinOp checkedPow e1 e2
  where
    checkedPow x y
      | y < 0 = failure "Negative exponent"
      | otherwise = pure $ x ^ y
eval (Eql e1 e2) = do
  x1 <- eval e1
  x2 <- eval e2
  case (x1, x2) of
    (ValInt x1', ValInt x2') -> pure $ ValBool $ x1' == x2'
    (ValBool x1', ValBool x2') -> pure $ ValBool $ x1' == x2'
    _ -> failure "Invalid operands to equality"
eval (If cond e1 e2) = do
  result <- eval cond
  case result of
    ValBool b -> eval (if b then e1 else e2)
    _ -> failure "Non-boolean conditional."
eval (Let var e1 e2) = do
  x1 <- eval e1
  localEnv (envExtend var x1) (eval e2)
eval (ForLoop (p, initial) (i, bound) body) = do
  initial' <- eval initial
  bound' <- eval bound
  case bound' of
    ValInt boundInt -> evalFor p i initial' 0 boundInt body
    _ -> failure "Non-integral loop bound"
eval (Lambda vname body) = do
  env <- askEnv
  pure $ ValFun env vname body
eval (Apply fexp arg) = do
  fexp' <- eval fexp
  arg' <- eval arg
  case fexp' of
    ValFun fenv vname body ->
      localEnv (\_ -> envExtend vname arg' fenv) (eval body)
    _ -> failure "Cannot apply non-functional expression"
eval (TryCatch body catchBody) = eval body `catch` eval catchBody

evalFor :: VName -> VName -> Val -> Integer -> Integer -> Exp -> EvalM Val
evalFor p i pval ival bound body
  | bound == ival = pure pval
  | otherwise =
      localEnv
        (envExtend p pval . envExtend i (ValInt ival))
        ( do
            bval <- eval body
            evalFor p i bval (ival + 1) bound body
        )
