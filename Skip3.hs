{-# LANGUAGE PatternGuards #-}
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

skipToN :: Ord a => Int -> a -> Skip a -> Skip a
skipToN i e baseL@(Base l) = goBase l (0::Int)
  where
    goBase [] _ = baseL
    goBase [_] _ = baseL
    goBase (_:rest@(second:_)) n
      | e <= second = baseL
      | n < i = goBase rest (n+1)
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

skipNums :: Skip Integer
skipNums = makeSkip [1..10]
-- skipEvens = makeSkip [0,2..10]

skipIn :: (Enum a, Num a, Ord a) => Skip a -> [(a, Bool)]
skipIn l = zip testList $ fmap (`skipElem` l) testList
   where
     testList = [(-5)..13]

depth :: Num b => Skip a -> b
depth (Base _)  = 1
depth (List []) = 1
depth (List (x:_)) = 1 + depth x
