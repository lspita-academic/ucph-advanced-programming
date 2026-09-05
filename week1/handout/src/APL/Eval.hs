module APL.Eval
  ( Val (..),
    EvalError (..),
    EvalResult,
    eval,
    envEmpty,
    envExtend,
  )
where

import APL.AST (Exp (Add, CstBool, CstInt, Div, Eql, If, Let, Mul, Pow, Sub, Var), VName)
import Data.Function ((&))
import GHC.Base (join)

data Val
  = ValInt Integer
  | ValBool Bool
  deriving (Eq, Show)

data EvalError
  = DivisionByZero
  | NegativeExponent
  | NonBooleanCondition
  | InvalidOperandsType
  | UndefinedReference VName
  deriving (Eq)

instance Show EvalError where
  show e = case e of
    DivisionByZero -> "Cannot divide by 0"
    NegativeExponent -> "Cannot use a negative exponent"
    NonBooleanCondition -> "Condition is not a boolean"
    InvalidOperandsType -> "Operands are not of a compatible type"
    UndefinedReference key -> "Undefined reference: " ++ key

type EvalResult = Either Val EvalError

type Env = [(VName, Val)]

envEmpty :: Env
envEmpty = []

envExtend :: VName -> Val -> Env -> Env
-- put the value in front for faster searches (time locality) and to override the
-- previous value because of linear lookup from the start
envExtend key val env = (key, val) : env

envLookup :: VName -> Env -> Maybe Val
envLookup key env = lookup key env

eval :: Env -> Exp -> EvalResult
eval _ (CstInt n) = Left $ ValInt n
eval _ (CstBool b) = Left $ ValBool b
eval env (Add e1 e2) =
  mapBinaryEval
    ( \x y -> case (x, y) of
        (ValInt x', ValInt y') -> safeIntBinaryOp (+) x' y'
        _ -> Right InvalidOperandsType
    )
    env
    e1
    e2
eval env (Sub e1 e2) =
  mapBinaryEval
    ( \x y -> case (x, y) of
        (ValInt x', ValInt y') -> safeIntBinaryOp (-) x' y'
        _ -> Right InvalidOperandsType
    )
    env
    e1
    e2
eval env (Mul e1 e2) =
  mapBinaryEval
    ( \x y -> case (x, y) of
        (ValInt x', ValInt y') -> safeIntBinaryOp (*) x' y'
        _ -> Right InvalidOperandsType
    )
    env
    e1
    e2
eval env (Div e1 e2) =
  mapBinaryEval
    ( \x y -> case (x, y) of
        (ValInt x', ValInt y') ->
          intBinaryOp
            ( \_ _ ->
                if y' == 0
                  then Just DivisionByZero
                  else Nothing
            )
            div
            x'
            y'
        _ -> Right InvalidOperandsType
    )
    env
    e1
    e2
eval env (Pow e1 e2) =
  mapBinaryEval
    ( \x y -> case (x, y) of
        (ValInt x', ValInt y') ->
          intBinaryOp
            ( \_ _ ->
                if y' < 0
                  then Just NegativeExponent
                  else Nothing
            )
            (^)
            x'
            y'
        _ -> Right InvalidOperandsType
    )
    env
    e1
    e2
eval env (Eql e1 e2) =
  mapBinaryEval
    ( \x y -> case (x, y) of
        (ValInt x', ValInt y') -> safeBoolBinaryOp (==) x' y'
        (ValBool x', ValBool y') -> safeBoolBinaryOp (==) x' y'
        _ -> Right InvalidOperandsType
    )
    env
    e1
    e2
eval env (If cond e1 e2) =
  mapEval
    ( \cond' -> case cond' of
        ValBool True -> eval env e1
        ValBool False -> eval env e2
        _ -> Right NonBooleanCondition
    )
    env
    cond
eval env (Var key) = envLookup key env & maybe (Right $ UndefinedReference key) Left
eval env (Let key e body) = mapEval (\val -> eval (envExtend key val env) body) env e

tryBinaryOp :: (t3 -> Val) -> Maybe (t1 -> t2 -> Maybe EvalError) -> (t1 -> t2 -> t3) -> t1 -> t2 -> EvalResult
tryBinaryOp wrapFn errorPredicate op x y =
  let -- join flattens out the `Maybe (Maybe EvalError)` into a `Maybe EvalError`
      -- https://hackage-content.haskell.org/package/base-4.22.0.0/docs/Control-Monad.html#v:join
      maybeError = errorPredicate & fmap (\f -> f x y) & join
   in case maybeError of
        Just e -> Right e
        Nothing -> Left (wrapFn $ op x y)

intBinaryOp :: (t1 -> t2 -> Maybe EvalError) -> (t1 -> t2 -> Integer) -> t1 -> t2 -> EvalResult
intBinaryOp errFn = tryBinaryOp ValInt (Just errFn)

safeIntBinaryOp :: (t1 -> t2 -> Integer) -> t1 -> t2 -> EvalResult
safeIntBinaryOp = tryBinaryOp ValInt Nothing

safeBoolBinaryOp :: (t1 -> t2 -> Bool) -> t1 -> t2 -> EvalResult
safeBoolBinaryOp = tryBinaryOp ValBool Nothing

mapEval :: (Val -> EvalResult) -> Env -> Exp -> EvalResult
mapEval mapFn env e = case eval env e of
  err@(Right _) -> err
  Left e' -> mapFn e'

mapBinaryEval :: (Val -> Val -> EvalResult) -> Env -> Exp -> Exp -> EvalResult
mapBinaryEval mapFn env e1 e2 = case (eval env e1, eval env e2) of
  (err@(Right _), _) -> err
  (_, err@(Right _)) -> err
  (Left e1', Left e2') -> mapFn e1' e2'
