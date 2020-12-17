{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}

module Main where

import Control.Arrow ((&&&))
import Control.Applicative (liftA3)
import Control.Monad (guard, replicateM)
import Control.Monad.ST (ST, runST)
import Data.Array.ST (STUArray, getAssocs, readArray, writeArray, newArray, getBounds, getElems)
import Data.Foldable (for_)
import Data.Ix (inRange)

type Coord = (Int, Int, Int)
type Cube s = STUArray s Coord Bool

iterateM :: Monad m => Int -> (a -> m a) -> a -> m a
iterateM 0 _f x = pure x
iterateM n f x = f x >>= iterateM (n - 1) f

neighbors :: Coord -> [Coord]
neighbors (x, y, z) = tail [(x+dx,y+dy,z+dz) | [dx, dy, dz] <- replicateM 3 [0, -1, 1]]

shouldLive :: Bool -> Int -> Bool
shouldLive alive numLivingNeighbors = case numLivingNeighbors of
  2 -> alive
  3 -> True
  _ -> False

tick :: Cube s -> ST s (Cube s)
tick cube = do
  bounds@((xmin, ymin, zmin), (xmax, ymax, zmax)) <- getBounds cube
  let bounds'@((xmin', ymin', zmin'), (xmax', ymax', zmax')) =
        ((xmin-1, ymin-1, zmin-1), (xmax+1, ymax+1, zmax+1))
      oldValue c | inRange bounds c = readArray cube c
                 | otherwise = pure False
  cube' <- newArray bounds' False
  for_ (liftA3 (,,) [xmin'..xmax'] [ymin'..ymax'] [zmin'..zmax']) $ \coord -> do
    let ns = neighbors coord
    living <- traverse oldValue ns
    curr <- oldValue coord
    writeArray cube' coord (shouldLive curr (length $ filter id living))
  pure cube'

test :: [(Coord, Bool)]
test = runST $ do
  cube <- newArray ((0,0,0), (1,1,0)) False :: forall s. ST s (STUArray s Coord Bool)
  for_ [(0,0,0), (1,1,0), (0,1,0)] $ (flip (writeArray cube) True)
  cube' <- iterateM 2 tick cube
  getAssocs cube'

type Input = ((Coord, Coord), [Coord])

part1 :: Input -> Int
part1 (bounds, alive) = runST $ do
  cube <- newArray bounds False
  for_ alive $ \c -> writeArray cube c True
  cube' <- iterateM 6 tick cube
  length . filter id <$> getElems cube'

part2 :: Input -> ()
part2 = const ()

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
