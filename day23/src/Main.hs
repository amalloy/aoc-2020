{-# LANGUAGE FlexibleContexts #-}
module Main where

import Control.Arrow ((&&&))
import Control.Monad.ST (ST, runST)
import Data.Char (digitToInt, intToDigit)
import Data.Foldable (for_)

import Data.Array.ST (STUArray, newArray, readArray, writeArray, getBounds)

type Input = [Int]
type Cups s = STUArray s Int Int

numCupsMoved :: Int
numCupsMoved = 3

iterateM :: Monad m => Int -> (a -> m a) -> a ->  m a
iterateM 0 _ x = pure x
iterateM n f x = f x >>= iterateM (n - 1) f

advance :: Cups s -> Int -> ST s Int
advance array curr = do
  (1, upper) <- getBounds array
  first <- readArray array curr
  second <- readArray array first
  third <- readArray array second
  firstImmobile <- readArray array third
  let movingCups = [first, second, third]
      destIndex = head . filter (not . (`elem` (0:movingCups))) . tail . iterate dec $ curr
      size = upper
      dec = succ . (`mod` size) . subtract 2
  after <- readArray array destIndex
  let writes = [(curr, firstImmobile), (third, after), (destIndex, first)]
  for_ writes $ uncurry (writeArray array)
  pure firstImmobile

initArray :: Int -> [Int] -> ST s (Int, Cups s)
initArray n xs@(x:_) = do
  array <- newArray (1, n) 0
  for_ ixs $ uncurry (writeArray array)
  pure (x, array)
  where ixs = seed ++ filler ++ finish
        finish | n == length xs = [(last xs, x)]
               | otherwise = [(last xs, length xs + 1), (n, x)]
        seed = zip xs (tail xs)
        filler = [(x, x + 1) | x <- [length xs + 1..n - 1]]

elemsFrom :: Int -> Cups s -> ST s [Int]
elemsFrom endpoint array = (endpoint:) <$> go endpoint
  where go focus = do
          x <- readArray array focus
          if x == endpoint
            then pure []
            else (x:) <$> go x

part1 :: Input -> String
part1 seed = runST $ do
  (first, array) <- initArray 9 seed
  _ <- iterateM 100 (advance array) first
  (1:elems) <- elemsFrom 1 array
  pure . map intToDigit $ elems

part2 :: Input -> Int
part2 seed = runST $ do
  (first, array) <- initArray 1000000 seed
  _ <- iterateM 10000000 (advance array) first
  a <- readArray array 1
  b <- readArray array a
  pure $ a * b

prepare :: String -> Input
prepare = map digitToInt . filter (/= '\n')

main :: IO ()
main = readFile "input.txt" >>= print . (part1 &&& part2) . prepare
