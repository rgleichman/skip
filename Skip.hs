import Data.List (intersperse, sort)
-- Takes a sorted list and makes a read only skip list of half the length
-- can be run recursively
-- The input list is sorted
-- a is the element, b is [] or the sublist


data PairList a = PairList a [PairList a] deriving (Show)

showPairList :: (Show a) => [PairList a] -> String
showPairList x = "[" ++ (concat $ intersperse "\n" (fmap (\y -> " " ++ printPairList y) x)) ++ "]"

printPairList :: (Show a) => PairList a -> String
printPairList (PairList x []) = show x
printPairList (PairList _ y) = " " ++ showPairList y

--makeTupleList :: Functor f => f a -> f (a, [b])
--makeTupleList :: Functor f => f a -> f (PairList a [a1])
makeTupleList :: Functor f => f a -> f (PairList a)
makeTupleList xs = fmap (\x -> PairList x []) xs

--skipHalf :: [a] -> [(a, [a])]
--skipHalf :: [a] -> [(a, [a])]
--skipHalf :: [(a,[(a,b)])] -> [(a,[(a,b)])]
--skipHalf :: [(a, b)] -> [(a, [(a, b)])]
skipHalf :: [PairList a] -> [PairList a]
skipHalf [] = []
--skipHalf [(x,y)] = [(x, [(x,y)])]
--skipHalf [Pair (x,y)] = [Pair (x,y)]
skipHalf [x] = [x]
--skipHalf l@((x,_):_:rest) = (x, l):(skipHalf rest)
skipHalf l@((PairList x _ ):_:rest) = (PairList x l):(skipHalf rest)

skipR :: [PairList a] -> [PairList a]
skipR [] = []
skipR [x] = [x]
skipR x = skipR (skipHalf x)

skipRList :: Ord a => [a] -> [PairList a]
skipRList = skipR . makeTupleList . sort


skipElem :: Ord a => a -> [PairList a] -> Bool
skipElem _ [] = False
skipElem x [PairList y z] = (x == y) || (skipElem x z)
skipElem x ((PairList y z):l@((PairList y2 _):_)) = (x == y) || (x==y2) || inLists
  where inLists
          | x < y2 = skipElem x z
          | otherwise = skipElem x l

--skipHalf l@(x:_:rest) = (x, l):skippedRest
--  where
--    skippedRest = skipHalf rest

foo :: [a] -> [a]
foo [] = []
foo [x] = [x]
foo (x:_:r) = x:foo r

-- --skipRecurse :: [a] -> ItemOrList a
-- skipRecurse [] = List []
-- skipRecurse (x:[]) = Item x
-- skipRecurse ls = skipRecurse halfOfList
--   where
--     --(List halfOfList) = skipHalf ls



sH :: [a] -> [a]
sH [] = []
sH (x:[]) = [x]
sH (x:_:rest) = x:(sH rest)
