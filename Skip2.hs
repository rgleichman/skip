{-# LANGUAGE PatternGuards #-}
module Skip2 (makeSkip
             , skipTo
             , Skip
             ,skipElem
             ,skipHalf
             ,skipToEight
             ,depth
             ) where

data Skip a = Item a | List [Skip a] deriving (Show)

makeSkip :: [a] -> [Skip a]
makeSkip = map Item

skipHalf :: [Skip a] -> [Skip a]
skipHalf [] = []
skipHalf [x] = [x]
skipHalf l@(_:_:rest) = List l:(skipHalf rest)

skipElem :: Ord a => a -> [Skip a] -> Bool
skipElem _ [] = False
skipElem x (Item y:rest)
  | x == y = True
  | otherwise = skipElem x rest
skipElem x [List y] = skipElem x y
skipElem _ (List _: Item _:_) =
  error "skip lists should not have Lists and Items on the same level"
skipElem x ((List first): l@(second: _))
  | Just val <- secondVal,
    x == val = True
  | Just val <- secondVal,
    x < val = skipElem x first
  | otherwise = skipElem x l
  where
    secondVal = getLeftVal second

-- Follows the tree to the left most leaf
getLeftVal :: Skip a -> Maybe a
getLeftVal (Item x) = Just x
getLeftVal (List []) = Nothing
getLeftVal (List (x:_)) = getLeftVal x


-- Builds a skip list at least as large as the given element.
-- Can be used to construct skip lists from infinite lists.
skipTo :: Ord a => a -> [Skip a] -> [Skip a]
skipTo _ [] = []
skipTo _ [x] = [x]
skipTo _ [x,y] = [x,y]
skipTo e l@(_:second:_)
  | Just val <- getLeftVal second
  ,e <= val = l
  | otherwise = skipTo e $ skipHalf l


--Like skipTo, but leaves off the top node with up to eight branches before the number
skipToEight :: Ord a => a -> [Skip a] -> [Skip a]
skipToEight _ [] = []
skipToEight _ [x] = [x]
skipToEight _ [x,y] = [x,y]
skipToEight e l = go l (0::Int)
  where
    --go :: Ord a => [Skip a] -> Int -> [Skip a]
    go [] _ = []
    go [x] _ = [x]
    go (_: rest@(second:_)) n
      | Just val <- getLeftVal second
      ,e <= val = l
      | n < 16 = (go rest (n+1))
      | otherwise = skipToEight e (skipHalf l)
    
-- skipNums = makeSkip [1..10]
-- skipEvens = makeSkip [0,2..10]

-- skipIn l = zip testList $ fmap (\x -> skipElem x l) testList
--   where
--     testList = [(-5)..13]

depth :: Num b => [Skip a] -> b
depth [] = 0
depth (Item _:_)  = 1
depth (List x:_) = 1 + depth x
