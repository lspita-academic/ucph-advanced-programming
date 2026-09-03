module APL.Eval
  ( Val (..),
    eval,
  )
where

import APL.AST (Exp (Add, CstInt, Div, Mul, Pow, Sub))

data Val
  = ValInt Integer
  deriving (Eq, Show)

binaryEval :: Exp -> Exp -> (Val, Val)
binaryEval x y = (eval x, eval y)

eval :: Exp -> Val
eval (CstInt n) = ValInt n
eval (Add x y) = case binaryEval x y of
  (ValInt x', ValInt y') -> ValInt (x' + y')
eval (Sub x y) = case binaryEval x y of
  (ValInt x', ValInt y') -> ValInt (x' - y')
eval (Mul x y) = case binaryEval x y of
  (ValInt x', ValInt y') -> ValInt (x' * y')
eval (Div x y) = case binaryEval x y of
  (ValInt x', ValInt y') -> ValInt (x' `div` y')
eval (Pow x y) = case binaryEval x y of
  (ValInt x', ValInt y') -> ValInt (x' ^ y')
