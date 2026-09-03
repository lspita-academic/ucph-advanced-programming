module APL.Eval
  ( Val (..),
    eval,
  )
where

import APL.AST (Exp (CstInt))

data Val
  = ValInt Integer
  deriving (Eq, Show)

eval :: Exp -> Val
eval (CstInt n) = ValInt n
