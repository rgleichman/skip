module Main (main) where

import Skip2
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
--main = main1
main = main3

addList :: [Integer]
--addList = [0,2..(80000)]
addList = [0,2..(20)]

-- takes 1.47 s with add list to 400, bigEvenNum = 2000000
-- With skipToN 16, now takes about 0.3 seconds
main1 :: IO ()
main1 = do
  --  putStrLn $ "eight depth: " ++ (show $ depth skippedEvens)
  --putStrLn $ "all depth: " ++ (show . depth . makeEvenSkip $ bigEvenNum )
  print $ and elems
  where
    --skippedEvens = skipHalfN 18 $ skipEvens -- 18 is 
    --skippedEvens = makeEvenSkip bigEvenNum
    skippedEvens = skipToN 16 bigEvenNum . makeSkip $ evens -- takes .43 s - .46s , depth 19
    elems = fmap (\x -> skipElem (bigEvenNum - x) skippedEvens) addList

main1a :: IO ()
main1a = print $ and elems
  where
    --skippedEvens = skipHalf. skipHalf. skipHalf. skipHalf. skipHalf. skipHalf . skipHalf. skipHalf . skipHalf . skipHalf . skipHalf $ skipEvens
    skippedEvens = skipHalfN 17 . makeSkip $ evens
    --skippedEvens = skipToEight bigEvenNum . makeSkip $ evens
    elems = fmap (\x -> skipElem (bigEvenNum - x) skippedEvens) addList

-- 0.18 sec
main1b :: IO ()
main1b = print $ and elems
  where
    skippedEvens =
      --Skip3.skipToN 16 bigEvenNum .
      Skip3.skipToN 16 bigEvenNum .
      Skip3.makeSkip $ evens -- takes .43 s - .46s , depth 19
    elems = fmap (\x -> Skip3.skipElem (bigEvenNum - x) skippedEvens) addList


skipHalfN :: Int -> [Skip a1] -> [Skip a1]
skipHalfN 1 l = l
skipHalfN n l = skipHalfN (n-1) (skipHalf l)

-- takes 3.4 s with add list to 400
main2 :: IO ()
main2 = print $ and elems
  where elems = fmap (\x -> elem (bigEvenNum - x) evens) addList


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
