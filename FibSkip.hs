module Main (main) where

import Skip
import Data.List (elem)

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

--bigEvenNum = 20000000
--bigEvenNum = 2000000
bigEvenNum = 500000

makeFibSkip x = skipTo x skipFibs

makeEvenSkip x = skipTo x skipEvens

-- skipTop :: [Skip a] -> [a]
-- skipTop = fmap (\(Skip x _) -> x)

-- eigthEvens = skipHalf. skipHalf . skipHalf $ skipEvens

-- depth :: Num b => [Skip a] -> b
-- depth [] = 0
-- depth (Skip _ x:_) = 1 + depth x

main :: IO ()
main = main1

addList :: [Integer]
addList = [0,2..(80000)]

-- takes 1.47 s with add list to 400 bigEvenNum = 2000000
main1 :: IO ()
main1 = print $ and elems
  where
    --skippedEvens = skipHalf. skipHalf. skipHalf. skipHalf. skipHalf. skipHalf . skipHalf. skipHalf . skipHalf . skipHalf . skipHalf $ skipEvens
    skippedEvens = makeEvenSkip bigEvenNum
    elems = fmap (\x -> skipElem (bigEvenNum - x) skippedEvens) addList

-- takes 3.4 s with add list to 400
main2 :: IO ()
main2 = print $ and elems
  where elems = fmap (\x -> elem (bigEvenNum - x) evens) addList
