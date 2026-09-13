import Control.Monad (filterM)

type Name = String

type Age = Int

data Country =
    Denmark
  | DPRK
  | Latveria

personAge :: Name -> Maybe Age
personAge "Troels" = Just 38
personAge "Toxo" = Just 8
personAge "Sebastian" = Just 29
personAge _ = Nothing

votingAge :: Country -> Maybe Age
votingAge Denmark = Just 18
votingAge DPRK = Just 17
votingAge Latveria = Nothing

mayVote0 :: Name -> Maybe Bool
mayVote0 name =
  fmap (\x -> x >= 18) (personAge name)

anotherPersonAge :: Name -> Either String Age
anotherPersonAge "Troels" = Right 38
anotherPersonAge "Doctor Doom" =
  Left "Doctor Doom is ageless!"
anotherPersonAge name =
  Left ("Unknown person: " ++ name)

anotherVotingAge :: Country -> Either String Age
anotherVotingAge Denmark = Right 18
anotherVotingAge DPRK = Right 17
anotherVotingAge Latveria = Left "Doctor Doom decides!"

data Box a = Box a
  deriving (Show)

instance Functor Box where
  fmap f (Box x) = Box (f x)

boxAge :: Name -> Box Age
boxAge _ = Box 42

mayVote1 ::
  (Functor f) =>
  (Name -> f Age) ->
  Name ->
  f Bool
mayVote1 getAge name =
  fmap (\x -> x >= 18) (getAge name)

mayVote2 ::
  Applicative f =>
  (Name -> f Age) ->
  (Country -> f Age) ->
  Name -> Country ->
  f Bool
mayVote2 getAge getMinAge name country =
  let -- age :: f Age
      age = getAge name
      -- min_age :: f Age
      min_age = getMinAge country
      cmp :: Int -> Int -> Bool
      cmp = (\x y -> x >= y)
      -- tmp :: f (Int -> Bool)
      tmp = fmap cmp age
      -- tmp2 :: f Bool
      tmp2 = tmp <*> min_age
  in tmp2

mayVote3 ::
  Applicative f =>
  (Name -> f Age) ->
  (Country -> f Age) ->
  Name -> Country ->
  f Bool
mayVote3 getAge getMinAge name country =
  (>=) <$> getAge name <*> getMinAge country

findVoters ::
  Monad f =>
  (Name -> f Age) ->
  (Country -> f Age) ->
  [Name] -> Country ->
  f [Name]
findVoters getAge getMinAge names country =
  case names of
    [] -> pure []
    name:remaining ->
      let
        -- tmp :: f Bool
        tmp = (>=) <$> getAge name <*> getMinAge country
        -- remaining' :: f [Name]
        remaining' =
          findVoters getAge getMinAge
          remaining country
        -- f :: Bool -> f [Name]
        f False = remaining'
        f True = fmap (\l -> name:l) remaining'
      in tmp >>= f

monadicFilter :: Monad m =>
                 (a -> m Bool)
              -> [a]
              -> m [a]
monadicFilter p [] = pure []
monadicFilter p (x:xs) =
  let keep = p x
      f False = monadicFilter p xs
      f True = (x:) <$> (monadicFilter p xs)
  in keep >>= f


-- (<$>) :: (a -> b) -> f a -> f b
-- (<*>) :: f (a -> b) -> f a -> f b
-- (>>=) :: f a -> (a -> f b) -> f b
-- (=<<) :: (a -> f b) -> f a -> f b

-- class Applicative m => Monad m where
--   (>>=) :: f a -> (a -> f b) -> f b

bindMaybe ::
  (a -> Maybe b) -> Maybe a -> Maybe b
bindMaybe f x =
  case x of
    Nothing -> Nothing
    Just x' -> f x'

bindEither ::
  (a -> Either b c)
  -> Either b a
  -> Either b c
bindEither f x =
  case x of
    Left x' -> Left x'
    Right x' -> f x'

-- class Functor f => Applicative2 f where
--  pure :: a -> f a
--  (<*>) :: f (a -> b) -> f a -> f b

-- instance Applicative2 Maybe where
--   -- pure :: a -> Maybe a
--   -- (<*>) :: Maybe (a -> b) -> Maybe a
--   --       -> Maybe b

--   -- x :: a
--   pure x = Just x

--   -- f :: Maybe (a -> b)
--   -- x :: Maybe a
--   -- f' :: a -> b
--   f <*> x = case (f, x) of
--               (Just f', Just x') ->
--                 Just (f' x')
--               _ -> Nothing

getAgeFromUser :: Name -> IO Age
getAgeFromUser name =
  putStrLn ("How old is " ++ name ++ "?")
  >>= \() ->
  getLine
  >>= \x ->
  pure (read x)

-- do x <- m1
--    m2
--
-- ==
--
-- m1 >>= \x -> m2
