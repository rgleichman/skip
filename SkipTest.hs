module Main (main) where

import Data.List (elem)
import qualified Data.Set as Set
import Data.List (elemIndex)
import Data.Maybe (fromJust)
import qualified Data.Map.Strict as Map
import qualified Skip3 as Skip3

fibs :: (Num a) => [a]
fibs = 0:1:zipWith (+) fibs (tail fibs)

--evens :: (Num a) => [a]
--evens = 0:fmap (+2) (evens)

--evens :: (Num a, Enum a) => [a]
evens :: [Integer]
evens = [0,2..]

--bigEvenNum = 2000000
--bigEvenNum = 500000
--bigEvenNum = 1000000
bigEvenNum :: Integer
bigEvenNum = 8000000
--bigEvenNum = 12000000

main :: IO ()
--main = main1
main = main3

addList :: [Integer]
--addList = [0,2..(80000)]
addList = [0,2..(20)]

-- 0.56 sec no profiling
-- 0.18 sec
-- 22369600 num bytes list allocates itself
--
main1 :: IO ()
main1 = print $ and elems
  where
    skippedEvens =
      --Skip3.skipToN 16 bigEvenNum .
      Skip3.skipToN 16 bigEvenNum .
      Skip3.makeSkip $ evens -- takes .43 s - .46s , depth 19
    elems = fmap (\x -> Skip3.skipElem (bigEvenNum - x) skippedEvens) addList

-- takes 3.4 s with add list to 400
main2 :: IO ()
main2 = print $ and elems
  where elems = fmap (\x -> elem (bigEvenNum - x) evens) addList


--No profiling: 1.75 s
-- 0.6 s
main3 :: IO ()
main3 = print $ and elems
  where
    bigEvenNumIndex = fromJust $ elemIndex bigEvenNum evens
    --bigEvenNumIndex = fromInteger $ bigEvenNum `div` 2
    set = Set.fromDistinctAscList $ take (1 + bigEvenNumIndex) evens
    elems = fmap (\x -> Set.member (bigEvenNum - x) set) addList


--This is slow
main4 :: IO ()
main4 = print $ and elems
  where
    bigEvenNumIndex = fromJust $ elemIndex bigEvenNum evens
    --bigEvenNumIndex = fromInteger $ bigEvenNum `div` 2
    set = Map.fromDistinctAscList . take (1 + bigEvenNumIndex) $ pairedEvens
    pairedEvens = fmap (\x -> (x,())) $ evens
    elems = fmap (\x -> Map.member (bigEvenNum - x) set) addList


main5 :: IO ()
main5 = print $ inList
  where
    inList = elem bigEvenNum evens
