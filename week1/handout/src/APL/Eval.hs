module APL.Eval
  ( Val (..),
    eval,
  )
where

import APL.AST (Exp (Add, CstInt, Div, Mul, Pow, Sub))
import Data.Function ((&))
import GHC.Base (join)

data Val
  = ValInt Integer
  deriving (Eq, Show)

type EvalError = String

type EvalResult = Either Val EvalError

eval :: Exp -> EvalResult
eval (CstInt n) = Left (ValInt n)
eval (Add e1 e2) =
  binaryEval
    ( tryCombine
        Nothing
        ( \x y -> case (x, y) of
            (ValInt x', ValInt y') -> ValInt (x' + y')
        )
    )
    e1
    e2
eval (Sub e1 e2) =
  binaryEval
    ( tryCombine
        Nothing
        ( \x y -> case (x, y) of
            (ValInt x', ValInt y') -> ValInt (x' - y')
        )
    )
    e1
    e2
eval (Mul e1 e2) =
  binaryEval
    ( tryCombine
        Nothing
        ( \x y -> case (x, y) of
            (ValInt x', ValInt y') -> ValInt (x' * y')
        )
    )
    e1
    e2
eval (Div e1 e2) =
  binaryEval
    ( tryCombine
        ( Just
            ( \x y -> case (x, y) of
                (ValInt _, ValInt y') -> if y' == 0 then Just "Cannot divide by 0" else Nothing
            )
        )
        ( \x y -> case (x, y) of
            (ValInt x', ValInt y') -> ValInt (x' `div` y')
        )
    )
    e1
    e2
eval (Pow e1 e2) =
  binaryEval
    ( tryCombine
        ( Just
            ( \x y -> case (x, y) of
                (ValInt _, ValInt y') -> if y' < 0 then Just "Cannot have a negative exponent" else Nothing
            )
        )
        ( \x y -> case (x, y) of
            (ValInt x', ValInt y') -> ValInt (x' ^ y')
        )
    )
    e1
    e2

tryCombine :: Maybe (Val -> Val -> Maybe EvalError) -> (Val -> Val -> Val) -> Val -> Val -> EvalResult
tryCombine errorPredicate mapFn x y = case errorPredicate & fmap (\f -> f x y) & join of
  Just e -> Right e
  Nothing -> Left (mapFn x y)

binaryEval :: (Val -> Val -> EvalResult) -> Exp -> Exp -> EvalResult
binaryEval mapFn x y = case (eval x, eval y) of
  (e@(Right _), _) -> e
  (_, e@(Right _)) -> e
  (Left x', Left y') -> mapFn x' y'
