module APL.AST
  ( VName,
    Exp (..),
    printExp,
  )
where

type VName = String

data Exp
  = CstInt Integer
  | CstBool Bool
  | Add Exp Exp
  | Sub Exp Exp
  | Mul Exp Exp
  | Div Exp Exp
  | Pow Exp Exp
  | Eql Exp Exp
  | If Exp Exp Exp
  | Var VName
  | Let VName Exp Exp
  | ForLoop (VName, Exp) (VName, Exp) Exp
  | Lambda VName Exp
  | Apply Exp Exp
  | TryCatch Exp Exp
  -- TODO: add cases
  deriving (Eq, Show)

addBrackets :: String -> String
addBrackets s = "(" ++ s ++ ")"

printInfix :: String -> Exp -> Exp -> String
printInfix op x y = addBrackets $ printExp x ++ " " ++ op ++ " " ++ printExp y

printExp :: Exp -> String
printExp (CstInt n) = show n
printExp (CstBool b) = show b
printExp (Add x y) = printInfix "+" x y
printExp (Sub x y) = printInfix "-" x y
printExp (Mul x y) = printInfix "*" x y
printExp (Div x y) = printInfix "/" x y
printExp (Pow x y) = printInfix "**" x y
printExp (Eql x y) = printInfix "==" x y
printExp (If cond e1 e2) =
  addBrackets $
    "if " ++ printExp cond ++ " then " ++ printExp e1 ++ " else " ++ printExp e2
printExp (Var vname) = vname
printExp (Let vname e body) =
  addBrackets $
    "let " ++ vname ++ " = " ++ printExp e ++ " in " ++ printExp body
printExp (ForLoop (p, initial) (i, bound) body) =
  addBrackets $
    "loop "
      ++ p
      ++ " = "
      ++ printExp initial
      ++ " for "
      ++ i
      ++ " < "
      ++ printExp bound
      ++ " do "
      ++ printExp body
printExp (Lambda arg body) = addBrackets $ "\\" ++ arg ++ " -> " ++ printExp body
printExp (Apply f x) = addBrackets $ printExp f ++ " " ++ printExp x
printExp (TryCatch x y) = addBrackets $ "try " ++ printExp x ++ " catch " ++ printExp y
