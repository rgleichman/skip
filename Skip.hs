module Skip (makeSkip
            , skipTo
            , Skip
            ,skipElem                   
            )  where

import Data.List (intercalate, sort)

data Skip a = Skip a [Skip a] deriving (Show)

-- Turn a list into a list of Skips.
-- The input should be sorted.
-- Compose with skipHalf or skipR.
makeSkip :: Functor f => f a -> f (Skip a)
makeSkip = fmap (\x -> Skip x [])

-- Make a full skip list from an unsorted list.
unsortedMakeSkipList :: Ord a => [a] -> [Skip a]
unsortedMakeSkipList = skipR . makeSkip . sort

-- Takes a sorted list of Skips and makes a read only skip list of half the length.
-- Can be run recursively.
skipHalf :: [Skip a] -> [Skip a]
skipHalf [] = []
skipHalf [x] = [x]
skipHalf l@(Skip x _:_:rest) = Skip x l : skipHalf rest

-- Recursively apply skipHalf. Makes a complete skip list.
-- Stops when the Skip tree for the skip list is fully formed
-- (ie. a "complete skipList" or when the list has only one element).
-- skipElem called on a complete skip list takes O(log n) time
-- where n is the length of the original list.
skipR :: [Skip a] -> [Skip a]
skipR [] = []
skipR [x] = [x]
skipR x = skipR (skipHalf x)

-- Builds a skip list at least as large as the given element.
-- Can be used to construct skip lists from infinite lists.
skipTo :: Ord a => a -> [Skip a] -> [Skip a]
skipTo _ [] = []
skipTo _ [x] = [x]
skipTo _ [x,y] = [x,y]
skipTo e l@(_:Skip y _:_)
  | e <= y = l
  | otherwise = skipTo e $ skipHalf l


-- Returns whether the item is in the list.
-- skipElem called on a complete skip list takes O(log n) time
-- where n is the length of the original list.
skipElem :: Ord a => a -> [Skip a] -> Bool
skipElem _ [] = False
skipElem x [Skip y z]
  | x == y = True
  | x < y = False
  | otherwise =  skipElem x z
skipElem x (Skip _ z : l@(Skip y2 _ : _)) = inLists
  where inLists
--          | x == y = True
          | x == y2 = True
--          | x < y = False
          | x < y2 = skipElem x z
          | otherwise = skipElem x l

-- Display a Skip list.
showListOfSkip :: (Show a) => [Skip a] -> String
showListOfSkip x = "[" ++ intercalate "\n" (fmap (\y -> " " ++ showSkip y) x) ++ "]"

-- Display a Skip
showSkip :: (Show a) => Skip a -> String
showSkip (Skip x []) = show x
showSkip (Skip _ y) = " " ++ showListOfSkip y
