module Main (main) where

import Skip2
import Data.List (elem)
import qualified Data.Set as Set
import Data.List (elemIndex)
import Data.Maybe (fromJust)

fibs :: (Num a) => [a]
fibs = 0:1:zipWith (+) fibs (tail fibs)

--evens :: (Num a) => [a]
--evens = 0:fmap (+2) (evens)

--evens :: (Num a, Enum a) => [a]
evens :: [Integer]
evens = [0,2..]

--skipEvens :: (Num a) => [Skip a]
skipEvens :: [Skip Integer]
skipEvens = makeSkip evens

skipFibs :: (Num a) => [Skip a]
skipFibs = makeSkip fibs

bigNum = 200000

slowElem x = skipElem (bigNum + x) skipFibs

--bigEvenNum = 2000000
--bigEvenNum = 500000
--bigEvenNum = 1000000
bigEvenNum = 8000000
--bigEvenNum = 12000000

makeFibSkip x = skipTo x skipFibs

makeEvenSkip x = skipTo x skipEvens

-- eigthEvens = skipHalf. skipHalf . skipHalf $ skipEvens

-- depth :: Num b => [Skip a] -> b
-- depth [] = 0
-- depth (Item _:_)  = 1
-- depth (List x:_) = 1 + depth x

main :: IO ()
main = main1

addList :: [Integer]
--addList = [0,2..(80000)]
addList = [0,2..(20)]

-- takes 1.47 s with add list to 400, bigEvenNum = 2000000
main1 :: IO ()
main1 = do
  --  putStrLn $ "eight depth: " ++ (show $ depth skippedEvens)
  --putStrLn $ "all depth: " ++ (show . depth . makeEvenSkip $ bigEvenNum )
  print $ and elems
  where
    --skippedEvens = skipHalfN 18 $ skipEvens -- 18 is 
    --skippedEvens = makeEvenSkip bigEvenNum
    skippedEvens = skipToEight bigEvenNum . makeSkip $ evens -- takes .43 s - .46s , depth 19
    elems = fmap (\x -> skipElem (bigEvenNum - x) skippedEvens) addList

main1a :: IO ()
main1a = print $ and elems
  where
    --skippedEvens = skipHalf. skipHalf. skipHalf. skipHalf. skipHalf. skipHalf . skipHalf. skipHalf . skipHalf . skipHalf . skipHalf $ skipEvens
    skippedEvens = skipHalfN 17 . makeSkip $ evens
    --skippedEvens = skipToEight bigEvenNum . makeSkip $ evens
    elems = fmap (\x -> skipElem (bigEvenNum - x) skippedEvens) addList

skipHalfN :: Int -> [Skip a1] -> [Skip a1]
skipHalfN 1 l = l
skipHalfN n l = skipHalfN (n-1) (skipHalf l)

-- takes 3.4 s with add list to 400
main2 :: IO ()
main2 = print $ and elems
  where elems = fmap (\x -> elem (bigEvenNum - x) evens) addList


main3 :: IO ()
main3 = print $ and elems
  where
    bigEvenNumIndex = fromJust $ elemIndex bigEvenNum evens
    --bigEvenNumIndex = fromInteger $ bigEvenNum `div` 2
    set = Set.fromDistinctAscList $ take (1 + bigEvenNumIndex) evens
    elems = fmap (\x -> Set.member (bigEvenNum - x) set) addList
