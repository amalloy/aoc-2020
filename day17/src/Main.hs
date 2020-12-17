{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}

module Main where

import Control.Arrow ((&&&))
import Control.Monad (guard, replicateM)
import Control.Monad.ST (ST, runST)
import Data.Array.ST (STUArray, readArray, writeArray, newArray, getBounds, getElems)
import Data.Foldable (for_)
import Data.Ix (Ix, inRange, range)

class Ix c => Coord c where
  neighbors :: c -> [c]
  plus :: Int -> c -> c

instance Coord (Int, Int, Int) where
  plus n (x, y, z) = (x+n, y+n, z+n)
  neighbors (x, y, z) = tail [(x+dx,y+dy,z+dz) |
                              [dx, dy, dz] <- replicateM 3 [0, -1, 1]]

instance Coord (Int, Int, Int, Int) where
  plus n (x, y, z, w) = (x+n, y+n, z+n, w+n)
  neighbors (x, y, z, w) = tail [(x+dx,y+dy,z+dz,w+dw) |
                                 [dx, dy, dz, dw] <- replicateM 4 [0, -1, 1]]

type Cube s c = STUArray s c Bool

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
    neighborStates <- traverse oldValue ns
    curr <- oldValue coord
    writeArray cube' coord (shouldLive curr (length $ filter id neighborStates))
  pure cube'

type Vec3 = (Int, Int, Int)
type Input = ((Vec3, Vec3), [Vec3])

run :: Coord c => ((c, c), [c]) -> Int
run (bounds, alive) = runST $ do
  cube <- newArray bounds False
  for_ alive $ \c -> writeArray cube c True
  cube' <- iterateM 6 tick cube
  length . filter id <$> getElems cube'

part1 :: Input -> Int
part1 = run

lift :: Vec3 -> (Int, Int, Int, Int)
lift (x, y, z) = (x, y, z, 0)

part2 :: Input -> Int
part2 ((lo, hi), alive) = run ((lift lo, lift hi), map lift alive)

prepare :: String -> Input
prepare s = let ls = lines s
                dim = length (head ls)
                bounds = ((0,0,0), (dim-1, dim-1, 0))
            in (bounds, do
                   (x, line) <- zip [0..] ls
                   (y, c) <- zip [0..] line
                   guard $ c == '#'
                   pure (x, y, 0))

main :: IO ()
main = readFile "input.txt" >>= print . (part1 &&& part2) . prepare
