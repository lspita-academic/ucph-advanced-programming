module APL.Eval
  ( Val (..),
    EvalError (..),
    EvalResult,
    eval,
  )
where

import APL.AST (Exp (Add, CstBool, CstInt, Div, Eql, If, Mul, Pow, Sub))
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
  deriving (Eq)

instance Show EvalError where
  show e = case e of
    DivisionByZero -> "Cannot divide by 0"
    NegativeExponent -> "Cannot use a negative exponent"
    NonBooleanCondition -> "Condition is not a boolean"
    InvalidOperandsType -> "Operands are not of a compatible type"

type EvalResult = Either Val EvalError

eval :: Exp -> EvalResult
eval (CstInt n) = Left $ ValInt n
eval (CstBool b) = Left $ ValBool b
eval (Add e1 e2) =
  mapBinaryEval
    ( \x y -> case (x, y) of
        (ValInt x', ValInt y') -> safeIntBinaryOp (+) x' y'
        _ -> Right InvalidOperandsType
    )
    e1
    e2
eval (Sub e1 e2) =
  mapBinaryEval
    ( \x y -> case (x, y) of
        (ValInt x', ValInt y') -> safeIntBinaryOp (-) x' y'
        _ -> Right InvalidOperandsType
    )
    e1
    e2
eval (Mul e1 e2) =
  mapBinaryEval
    ( \x y -> case (x, y) of
        (ValInt x', ValInt y') -> safeIntBinaryOp (*) x' y'
        _ -> Right InvalidOperandsType
    )
    e1
    e2
eval (Div e1 e2) =
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
    e1
    e2
eval (Pow e1 e2) =
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
    e1
    e2
eval (Eql e1 e2) =
  mapBinaryEval
    ( \x y -> case (x, y) of
        (ValInt x', ValInt y') -> safeBoolBinaryOp (==) x' y'
        (ValBool x', ValBool y') -> safeBoolBinaryOp (==) x' y'
        _ -> Right InvalidOperandsType
    )
    e1
    e2
eval (If cond e1 e2) =
  mapEval
    ( \cond' -> case cond' of
        ValBool True -> eval e1
        ValBool False -> eval e2
        _ -> Right NonBooleanCondition
    )
    cond

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

mapEval :: (Val -> EvalResult) -> Exp -> EvalResult
mapEval mapFn e = case eval e of
  err@(Right _) -> err
  Left e' -> mapFn e'

mapBinaryEval :: (Val -> Val -> EvalResult) -> Exp -> Exp -> EvalResult
mapBinaryEval mapFn e1 e2 = case (eval e1, eval e2) of
  (err@(Right _), _) -> err
  (_, err@(Right _)) -> err
  (Left e1', Left e2') -> mapFn e1' e2'
