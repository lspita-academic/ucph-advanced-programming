module APL.Check (checkExp, Error) where

import APL.AST (Exp (..), VName)
import Control.Monad (ap, liftM)

type Error = String

type State = [VName]

newtype CheckM a = CheckM (State -> (Either Error a))

instance Functor CheckM where
  fmap = liftM

instance Applicative CheckM where
  (<*>) = ap
  pure x = CheckM $ \_ -> (Right x)

instance Monad CheckM where
  CheckM m >>= f = CheckM $ \s ->
    let res = m s
     in case res of
          Left err -> Left err
          Right x ->
            let CheckM m' = f x
             in m' s

stateHasKey :: VName -> State -> Bool
stateHasKey = elem

stateExtend :: VName -> State -> State
stateExtend k s = k : s

askState :: CheckM State
askState = CheckM $ \s -> Right s

localState :: (State -> State) -> CheckM a -> CheckM a
localState f (CheckM m) = CheckM $ \s -> m (f s)

failure :: Error -> CheckM a
failure err = CheckM $ \_ -> Left err

checkMany :: [Exp] -> CheckM ()
checkMany [] = pure ()
checkMany (x : xs) = do
  check x
  checkMany xs

check :: Exp -> CheckM ()
check (Let k e body) = do
  check e
  localState (stateExtend k) (check body)
check (ForLoop (p, initial) (i, bound) body) = do
  check initial
  check bound
  localState (stateExtend p . stateExtend i) (check body)
check (Lambda k body) = localState (stateExtend k) (check body)
check (Var k) = do
  s <- askState
  if stateHasKey k s then pure () else failure $ "Variable not in scope: " ++ k
check (Add l r) = checkMany [l, r]
check (Sub l r) = checkMany [l, r]
check (Mul l r) = checkMany [l, r]
check (Div l r) = checkMany [l, r]
check (Pow l r) = checkMany [l, r]
check (Eql l r) = checkMany [l, r]
check (If cond e1 e2) = checkMany [cond, e1, e2]
check (Apply e1 e2) = checkMany [e1, e2]
check (TryCatch e1 e2) = checkMany [e1, e2]
check (Print _ e) = check e
check (KvPut e1 e2) = checkMany [e1, e2]
check (KvGet e) = check e
check (CstInt _) = pure ()
check (CstBool _) = pure ()

runCheckM :: CheckM a -> Either Error a
runCheckM (CheckM m) = m []

checkExp :: Exp -> Maybe Error
checkExp e = case runCheckM (check e) of
  Left err -> Just err
  Right _ -> Nothing
