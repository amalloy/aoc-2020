{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveFunctor #-}

module Main where

import Control.Monad (guard)
import Control.Monad.ST (ST, runST)
import Data.Array.ST (STUArray, readArray, writeArray, newArray, getBounds, getElems)
import Data.Foldable (for_)
import Data.Ix (Ix, inRange, range)
import Data.List.NonEmpty (NonEmpty(..))

type Vec2 = (Int, Int)
data Parameters a = Parameters (a, a) [a] deriving Functor
type Input = Parameters Vec2
type Cube s i = STUArray s i Bool

{-
Laws:
1. plus 0 x = x (follows from 2&4)
2. plus a (plus b x) = plus (a + b) x
3. plus a (plus b x) = plus b (plus a x)
4. plus a (plus (negate a) x) = x
5. forall c n ns.
     neighbors c = (n :| ns) =>
       n = c
 -}
class Ix c => Coord c where
  neighbors :: c -> NonEmpty c
  plus :: Int -> c -> c

instance Coord Int where
  plus = (+)
  neighbors = sequenceA $ id :| [pred, succ]

instance Coord c => Coord (Int, c) where
  plus n (x, c) = (plus n x, plus n c)
  neighbors (x, c) = do
    n <- neighbors c
    delta <- 0 :| [-1, 1]
    pure (x `plus` delta, n)

nextDimension :: a -> (Int, a)
nextDimension n = (0, n)

iterateM :: Monad m => Int -> (a -> m a) -> a -> m a
iterateM 0 _f x = pure x
iterateM n f x = f x >>= iterateM (n - 1) f

shouldLive :: Bool -> Int -> Bool
shouldLive alive numLivingNeighbors = case numLivingNeighbors of
  2 -> alive
  3 -> True
  _ -> False

tick :: Coord c => Cube s c -> ST s (Cube s c)
tick cube = do
  bounds@(lo, hi) <- getBounds cube
  let bounds' = (plus (-1) lo, plus 1 hi)
      oldValue c | inRange bounds c = readArray cube c
                 | otherwise = pure False
  cube' <- newArray bounds' False
  for_ (range bounds') $ \coord -> do
    let ns = neighbors coord
    self :| others <- traverse oldValue ns
    writeArray cube' coord (shouldLive self (length $ filter id others))
  pure cube'

run :: Coord c => Parameters c -> Int
run (Parameters bounds alive) = runST $ do
  cube <- newArray bounds False
  for_ alive $ \c -> writeArray cube c True
  cube' <- iterateM 6 tick cube
  length . filter id <$> getElems cube'

part1 :: Input -> Int
part1 = run . fmap nextDimension

part2 :: Input -> Int
part2 = run . fmap (nextDimension . nextDimension)

extraCredit :: Input -> Int
extraCredit = run . fmap (nextDimension . nextDimension . nextDimension)

prepare :: String -> Input
prepare s = let ls = lines s
                dim = length (head ls)
                bounds = ((0,0), (dim-1, dim-1))
            in Parameters bounds $ do
                   (x, line) <- zip [0..] ls
                   (y, c) <- zip [0..] line
                   guard $ c == '#'
                   pure (x, y)

main :: IO ()
main = readFile "input.txt" >>= print . sequence [part1, part2, extraCredit] . prepare
