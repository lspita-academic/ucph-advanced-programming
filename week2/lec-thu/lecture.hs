import Control.Monad (liftM, ap)

data Tree a =
    Leaf a
  | Node (Tree a) (Tree a)
  deriving (Show)

someTree :: Tree Int
someTree =
  Node (Leaf 2)
       (Node (Node (Leaf 1)
                   (Leaf 0))
             (Leaf 5))

--       *
--      / \
--     2   *
--        / \
--       *   5
--      / \
--     1   0

transformTree :: Int -> Tree Int -> Tree Bool
transformTree x (Leaf y) =
  Leaf (y >= x)
transformTree x (Node lc rc) =
  Node (transformTree x lc)
       (transformTree x rc)

data Reader r a = Reader (r -> a)


ask :: Reader r r
ask = Reader (\y -> y)

runReader :: Reader r a -> r -> a
runReader (Reader f) y = f y

-- same as liftM
fmapMonad :: Monad m => (a -> b) -> m a -> m b
fmapMonad f x =
  x >>= \x' -> pure (f x')

-- same as 'ap'
applyMonad :: Monad m =>
              m (a -> b) -> m a -> m b
applyMonad f x =
  f >>= \f' -> x >>= \x' -> pure (f' x')

instance Functor (Reader r) where
  fmap = liftM

instance Applicative (Reader r) where
  -- pure :: a -> Reader r a
  -- x :: a
  -- want: Reader r a
  pure x =
    Reader (\_ -> x)

  (<*>) = ap

instance Monad (Reader r) where
  -- (>>=) :: Reader r a
  --       -> (a -> Reader r b)
  --       -> Reader r b
  --
  -- have:
  --  x :: Reader r a
  --  f :: a -> Reader r b
  x >>= f =
    -- x' :: r -> a
    let Reader x' = x
        -- tmp :: r -> b
        tmp = \y ->
          -- tmp2 :: a
          let tmp2 = x' y
              -- tmp3 :: r -> b
              Reader tmp3 = f tmp2
          in tmp3 y
    in Reader tmp

transformTreeM :: Tree Int
               -> Reader Int (Tree Bool)
transformTreeM (Leaf x) = do
  y <- ask
  pure (Leaf (x >= y))
transformTreeM (Node lc rc) = do
  lc' <- transformTreeM lc
  rc' <- transformTreeM rc
  pure (Node lc' rc')

-- x >>= \x' -> m
--
-- write as
--
-- do x' <- x
--    m

data State s a = State (s -> (a,s))

instance Functor (State s) where
  fmap = liftM

instance Applicative (State s) where
  (<*>) = ap
  pure x = State (\s -> (x,s))

instance Monad (State s) where
  -- (>>=) :: State s a
  --       -> (a -> State s b)
  --       -> State s b
  -- have:
  --  x' :: s -> (a,s)
  --  s :: s
  --  x'' :: a
  --  s' :: s
  --  y :: s -> (b,s)
  --  y' :: b
  --  s'' = s
  x >>= f = State (\s ->
                     let State x' = x
                         (x'', s') = x' s
                         State y = f x''
                         (y', s'') = y s'
                     in (y', s''))

put :: s -> State s ()
put s = State (\_ -> ((), s))

get :: State s s
get = State (\s -> (s,s))

runState :: State s a -> s -> (a, s)
runState (State f) s = f s

frobTreeM :: Tree Int -> State Int (Tree Int)
frobTreeM (Leaf x) = do
  s <- get
  put (s+1)
  pure (Leaf (x+s))
frobTreeM (Node lc rc) = do
  lc' <- frobTreeM lc
  rc' <- frobTreeM rc
  pure (Node lc' rc')

frobTreeT :: Tree Int -> State Int (Tree Int)
frobTreeT = traverse (\x -> do
                        s <- get
                        put (s+1)
                        pure (x+s))

frobTree :: Int -> Tree Int -> (Tree Int, Int)
frobTree cnt (Leaf x) =
  (Leaf (x+cnt), cnt+1)
frobTree cnt (Node lc rc) =
  let (lc', cnt') = frobTree cnt lc
      (rc', cnt'') = frobTree cnt' rc
  in (Node lc' rc', cnt'')

instance Functor Tree where
  fmap f t = runIdentity (traverse f' t)
    where f' x = pure (f x)

data Identity a = Identity a

runIdentity :: Identity a -> a
runIdentity (Identity x) = x

instance Functor Identity where
  fmap = liftM

instance Applicative Identity where
  pure x = Identity x
  (<*>) = ap

instance Monad Identity where
  Identity x >>= f = f x

instance Foldable Tree where
  -- foldr: (a -> b -> b) -> b -> t a -> b
  foldr = traversableFoldR

traversableFoldR :: Traversable t =>
                    (a -> b -> b) -> b -> t a -> b
traversableFoldR op initial_acc t =
    let (_, final_acc) =
          runState (traverse onElem t) initial_acc
    in final_acc
    where onElem a =
            (\acc -> op a acc) <$> get

instance Traversable Tree where
  -- traverse :: Applicative m =>
  --             (a -> m b)
  --           -> Tree a
  --           -> m (Tree b)
  --
  -- have:
  --  f :: a -> m b
  --  t :: Tree a
  --  x :: a
  --  lc :: Tree a
  --  rc :: Tree a
  traverse f t = case t of
    Leaf x -> fmap Leaf (f x)
    Node lc rc ->
      -- lc' :: m (Tree b)
      -- rc' :: m (Tree b)
      let lc' = traverse f lc
          rc' = traverse f rc
          -- tmp :: m (Tree b -> Tree b -> Tree b)
          tmp = pure Node
          -- tmp2 :: m (Tree b -> Tree b)
          tmp2 = tmp <*> lc'
          -- tmp3 :: m (Tree b)
          tmp3 = tmp2 <*> rc'
      in tmp3

