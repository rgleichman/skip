{-# LANGUAGE PatternGuards , BangPatterns#-}
module Skip3 (makeSkip
--             , skipTo
             , skipToN
             , Skip
             ,skipElem
--             ,skipHalf
--             ,skipToEight
             ,depth
             ,skipN
             ) where

data Skip a = Base [a] | List [Skip a] deriving (Show)

makeSkip :: [a] -> Skip a
makeSkip = Base

{-
skipHalf :: [Skip a] -> [Skip a]
skipHalf [] = []
skipHalf [x] = [x]
skipHalf l@(_:_:rest) = List l:(skipHalf rest)
-}

--skipHalf :: [Skip a] -> [Skip a]
--skipHalf = skipN 2

--skipHalf = skipN 8

--skipN :: Integer -> [Skip a] -> [Skip a]
-- skipN :: Int -> [Skip a] -> [Skip a]
-- skipN _ [] = []
-- skipN _ [x] = [x]
-- skipN n l
--   | n < 1 = l
--   |otherwise = List l: (go n l)
--     where
--       go 0 goL = (List goL): go n goL
--       go i (_:rest) = go (i-1) rest
--       go _ [] = []
      --go [] = []
      --go goL = List next : (go next)
      --  where next = drop n goL

skipN :: Int -> Skip a -> Skip a
skipN n skip
  | n < 1 = skip
  | otherwise = case skip of
    uL@(List l) -> List $ uL: skipNUpper n l
    b@(Base l) -> List $ b:skipNBase n l
  where
    skipNUpper 0 upperL = List upperL:skipNUpper n upperL
    skipNUpper i (_:rest) = skipNUpper (i-1) rest
    skipNUpper _ [] = []
    skipNBase 0 baseL = Base baseL:skipNBase n baseL
    skipNBase i (_:rest) = skipNBase (i-1) rest
    skipNBase _ [] = []

skipElem :: Ord a => a -> Skip a -> Bool
skipElem element l = case l of
  Base baseList -> findInBase baseList
  List upperList -> findInUpper upperList
  where
    findInUpper [] = False
    findInUpper [x] = skipElem element x
    findInUpper (first: secondList@(second:_)) =
      case getLeftVal second of
        Just leftVal -> equalOrLess
          where equalOrLess
                  | element == leftVal = True
                  | element < leftVal = skipElem element first
                  | otherwise = findInUpper secondList
        Nothing -> False
    findInBase [] = False
    findInBase (x:rest)
      | element == x = True
      | element < x = False
      | otherwise = findInBase rest
    
-- Follows the tree to the left most leaf
getLeftVal :: Skip a -> Maybe a
getLeftVal (Base []) = Nothing
getLeftVal (Base (x:_)) = Just x
getLeftVal (List []) = Nothing
getLeftVal (List (x:_)) = getLeftVal x


-- Builds a skip list at least as large as the given element.
-- Can be used to construct skip lists from infinite lists.
-- skipTo :: Ord a => a -> [Skip a] -> [Skip a]
-- skipTo _ [] = []
-- skipTo _ [x] = [x]
-- skipTo _ [x,y] = [x,y]
-- skipTo e l@(_:second:_)
--   | Just val <- getLeftVal second
--   ,e <= val = l
--   | otherwise = skipTo e $ skipHalf l

 --Like skipTo, but leaves off the top node with up to eight branches before the number
-- skipToEight :: Ord a => a -> [Skip a] -> [Skip a]
-- skipToEight _ [] = []
-- skipToEight _ [x] = [x]
-- skipToEight _ [x,y] = [x,y]
-- skipToEight e l = go l (0::Int)
--   where
--     --go :: Ord a => [Skip a] -> Int -> [Skip a]
--     go [] _ = []
--     go [x] _ = [x]
--     go (_: rest@(second:_)) n
--       | Just val <- getLeftVal second
--       ,e <= val = l
--       | n < 16 = (go rest (n+1))
--       | otherwise = skipToEight e (skipHalf l)

-- skipToN :: Ord a => Int -> a -> [Skip a] -> [Skip a]
-- skipToN _ _ [] = []
-- skipToN _ _ [x] = [x]
-- skipToN _ _ [x,y] = [x,y]
-- skipToN i e l = go l (0::Int)
--   where
--     --go :: Ord a => [Skip a] -> Int -> [Skip a]
--     go [] _ = []
--     go [x] _ = [x]
--     go (_: rest@(second:_)) n
--       | Just val <- getLeftVal second
--       ,e <= val = l
--       | n < i = (go rest (n+1))
--       | otherwise = skipToN i e (skipN i l)

skipToN :: Ord a => Int -> a -> Skip a -> Skip a
skipToN i e baseL@(Base l) = goBase l (0::Int)
  where
    goBase [] _ = baseL
    goBase [_] _ = baseL
    goBase (_:rest@(second:_)) n
      | e <= second = baseL
      | n < i = (goBase rest (n+1))
      | otherwise = skipToN i e (skipN i baseL)
skipToN i e upperL@(List l) = goUpper l (0::Int)
  where
    goUpper [] _= upperL
    goUpper [_] _ = upperL
    goUpper (_:rest@(second:_)) n
      | Just leftVal <- getLeftVal second
      , e <= leftVal = upperL
      | n < i = goUpper rest (n+1)
      | otherwise = skipToN i e (skipN i upperL)

skipNums = makeSkip [1..10]
-- skipEvens = makeSkip [0,2..10]

skipIn l = zip testList $ fmap (\x -> skipElem x l) testList
   where
     testList = [(-5)..13]

depth :: Num b => Skip a -> b
depth (Base _)  = 1
depth (List []) = 1
depth (List (x:_)) = 1 + depth x
