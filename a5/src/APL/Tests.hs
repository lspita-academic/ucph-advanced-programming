module APL.Tests
  ( properties,
  )
where

import APL.AST (Exp (..), VName, printExp, subExp)
import APL.Check (checkExp)
import APL.Error (isDomainError, isTypeError, isVariableError)
import APL.Eval (eval, runEval)
import APL.Parser (keywords, parseAPL)
import Test.QuickCheck
  ( Arbitrary (arbitrary, shrink),
    Gen,
    Positive (getPositive),
    Property,
    checkCoverage,
    choose,
    cover,
    elements,
    frequency,
    listOf1,
    oneof,
    property,
    sized,
    vectorOf,
    withNumTests,
  )

instance Arbitrary Exp where
  arbitrary = sized (genExp [])

  shrink (Add e1 e2) =
    e1 : e2 : [Add e1' e2 | e1' <- shrink e1] ++ [Add e1 e2' | e2' <- shrink e2]
  shrink (Sub e1 e2) =
    e1 : e2 : [Sub e1' e2 | e1' <- shrink e1] ++ [Sub e1 e2' | e2' <- shrink e2]
  shrink (Mul e1 e2) =
    e1 : e2 : [Mul e1' e2 | e1' <- shrink e1] ++ [Mul e1 e2' | e2' <- shrink e2]
  shrink (Div e1 e2) =
    e1 : e2 : [Div e1' e2 | e1' <- shrink e1] ++ [Div e1 e2' | e2' <- shrink e2]
  shrink (Pow e1 e2) =
    e1 : e2 : [Pow e1' e2 | e1' <- shrink e1] ++ [Pow e1 e2' | e2' <- shrink e2]
  shrink (Eql e1 e2) =
    e1 : e2 : [Eql e1' e2 | e1' <- shrink e1] ++ [Eql e1 e2' | e2' <- shrink e2]
  shrink (If cond e1 e2) =
    e1 : e2 : [If cond' e1 e2 | cond' <- shrink cond] ++ [If cond e1' e2 | e1' <- shrink e1] ++ [If cond e1 e2' | e2' <- shrink e2]
  shrink (Let x e1 e2) =
    e1 : [Let x e1' e2 | e1' <- shrink e1] ++ [Let x e1 e2' | e2' <- shrink e2]
  shrink (Lambda x e) =
    [Lambda x e' | e' <- shrink e]
  shrink (Apply e1 e2) =
    e1 : e2 : [Apply e1' e2 | e1' <- shrink e1] ++ [Apply e1 e2' | e2' <- shrink e2]
  shrink (TryCatch e1 e2) =
    e1 : e2 : [TryCatch e1' e2 | e1' <- shrink e1] ++ [TryCatch e1 e2' | e2' <- shrink e2]
  shrink _ = []

halfSize :: Int -> Int
halfSize size = size `div` 2

thirdSize :: Int -> Int
thirdSize size = size `div` 3

genVar :: Gen VName
genVar = do
  var <- frequency [(1, genVarTrivial), (4, genVarNonTrivial)]
  if var `elem` keywords then genVar else pure var

genVarTrivial :: Gen VName
genVarTrivial = do
  alpha <- elements ['a' .. 'z']
  n <- choose (4, 10)
  alphaNums <- vectorOf n $ elements (['a' .. 'z'] ++ ['0' .. '9'])
  elements [[alpha], alpha : alphaNums]

genVarNonTrivial :: Gen VName
genVarNonTrivial = do
  n <- choose (2, 4)
  take n <$> genVarAtLeast2
  where
    genVarAtLeast2 = do
      alpha <- elements ['a' .. 'z']
      alphaNums <- listOf1 $ elements $ ['a' .. 'z'] ++ ['0' .. '9']
      pure (alpha : alphaNums)

genDomainError :: [VName] -> Int -> Gen Exp
genDomainError vars size =
  oneof
    [ Div <$> genExp vars (halfSize size) <*> pure (CstInt 0),
      Pow <$> genExp vars (halfSize size) <*> (Sub (CstInt 0) <$> (CstInt . getPositive <$> arbitrary))
    ]

data ExpType = Bool | Int | Fun
  deriving (Eq)

genNotOfType :: [ExpType] -> [VName] -> Int -> Gen Exp
genNotOfType t vars size =
  let typeMaps =
        [ (Bool, CstBool <$> arbitrary),
          (Int, CstInt . abs <$> arbitrary),
          (Fun, Lambda <$> genVar <*> genExp vars (size - 1))
        ]
      valid = filter (\(t', _) -> t' `elem` t) typeMaps
      generators = map snd valid
   in oneof generators

genTypeError :: [VName] -> Int -> Gen Exp
genTypeError vars size =
  oneof
    [ -- NonInteger
      Add <$> genNotOfType [Int] vars (halfSize size) <*> genExp vars (halfSize size),
      Add <$> genExp vars (halfSize size) <*> genNotOfType [Int] vars (halfSize size),
      Add <$> genNotOfType [Int] vars (halfSize size) <*> genNotOfType [Int] vars (halfSize size),
      Sub <$> genNotOfType [Int] vars (halfSize size) <*> genExp vars (halfSize size),
      Sub <$> genExp vars (halfSize size) <*> genNotOfType [Int] vars (halfSize size),
      Sub <$> genNotOfType [Int] vars (halfSize size) <*> genNotOfType [Int] vars (halfSize size),
      Mul <$> genNotOfType [Int] vars (halfSize size) <*> genExp vars (halfSize size),
      Mul <$> genExp vars (halfSize size) <*> genNotOfType [Int] vars (halfSize size),
      Mul <$> genNotOfType [Int] vars (halfSize size) <*> genNotOfType [Int] vars (halfSize size),
      Div <$> genNotOfType [Int] vars (halfSize size) <*> genExp vars (halfSize size),
      Div <$> genExp vars (halfSize size) <*> genNotOfType [Int] vars (halfSize size),
      Div <$> genNotOfType [Int] vars (halfSize size) <*> genNotOfType [Int] vars (halfSize size),
      Pow <$> genNotOfType [Int] vars (halfSize size) <*> genExp vars (halfSize size),
      Pow <$> genExp vars (halfSize size) <*> genNotOfType [Int] vars (halfSize size),
      Pow <$> genNotOfType [Int] vars (halfSize size) <*> genNotOfType [Int] vars (halfSize size),
      -- InvalidEqual
      Eql <$> (CstBool <$> arbitrary) <*> (CstInt . abs <$> arbitrary),
      Eql <$> (CstInt . abs <$> arbitrary) <*> (CstBool <$> arbitrary),
      Eql <$> genNotOfType [Int, Bool] vars (halfSize size) <*> genNotOfType [Int, Bool] vars (halfSize size),
      -- NonBoolean
      If <$> genNotOfType [Bool] vars (thirdSize size) <*> genExp vars (thirdSize size) <*> genExp vars (thirdSize size),
      -- NonFunction
      Apply <$> genNotOfType [Fun] vars size <*> genExp vars (halfSize size)
    ]

genVariableError :: [VName] -> Int -> Gen Exp
genVariableError vars _ =
  Var <$> genUndefinedVar
  where
    genUndefinedVar = do
      v <- genVar
      if v `elem` vars then genUndefinedVar else pure v

genExp :: [VName] -> Int -> Gen Exp
genExp _ 0 = oneof [CstInt . abs <$> arbitrary, CstBool <$> arbitrary]
genExp vars size =
  frequency
    [ (40, CstInt . abs <$> arbitrary),
      (20, CstBool <$> arbitrary),
      (40, Add <$> genExp vars (halfSize size) <*> genExp vars (halfSize size)),
      (40, Sub <$> genExp vars (halfSize size) <*> genExp vars (halfSize size)),
      (40, Mul <$> genExp vars (halfSize size) <*> genExp vars (halfSize size)),
      (20, Div <$> genExp vars (halfSize size) <*> genExp vars (halfSize size)),
      (20, Pow <$> genExp vars (halfSize size) <*> genExp vars (halfSize size)),
      (15, Eql <$> genExp vars (halfSize size) <*> genExp vars (halfSize size)),
      (25, If <$> genExp vars (thirdSize size) <*> genExp vars (thirdSize size) <*> genExp vars (thirdSize size)),
      (if null vars then 0 else 200, Var <$> elements vars),
      ( 150,
        do
          x <- genVar
          Let x <$> genExp vars (halfSize size) <*> genExp (x : vars) (halfSize size)
      ),
      ( 150,
        do
          x <- genVar
          Lambda x <$> genExp (x : vars) (size - 1)
      ),
      (10, Apply <$> genExp vars (halfSize size) <*> genExp vars (halfSize size)),
      (30, TryCatch <$> genExp vars (halfSize size) <*> genExp vars (halfSize size)),
      (5, genDomainError vars size),
      (5, genTypeError vars size),
      (5, genVariableError vars size)
    ]

expCoverage :: Exp -> Property
expCoverage e =
  checkCoverage
    . cover 20 (any isDomainError (checkExp e)) "domain error"
    . cover 20 (not $ any isDomainError (checkExp e)) "no domain error"
    . cover 20 (any isTypeError (checkExp e)) "type error"
    . cover 20 (not $ any isTypeError (checkExp e)) "no type error"
    . cover 5 (any isVariableError (checkExp e)) "variable error"
    . cover 70 (not $ any isVariableError (checkExp e)) "no variable error"
    . cover 50 (or [2 <= n && n <= 4 | Var v <- subExp e, let n = length v]) "non-trivial variable"
    $ ()

parsePrinted :: Exp -> Bool
parsePrinted e = case parseAPL "" (printExp e) of
  Left _ -> False
  Right e' -> e' == e

onlyCheckedErrors :: Exp -> Bool
onlyCheckedErrors e =
  let possibleErrors = checkExp e
   in case runEval (eval e) of
        Left err -> err `elem` possibleErrors
        Right _ -> True

-- The number of tests is part of the specification of this test suite: some of
-- these properties fail only rarely.  Do not reduce it.
--
-- In the use of ‘withMaxSuccess’
-- (imported from Test.QuickCheck, but defined in Test.QuickCheck.Property):
-- Deprecated: "Use `withNumTests` instead"
properties :: [(String, Property)]
properties =
  [ ("expCoverage", property $ withNumTests 10000 expCoverage),
    ("parsePrinted", property $ withNumTests 10000 parsePrinted),
    ("onlyCheckedErrors", property $ withNumTests 10000 onlyCheckedErrors)
  ]
