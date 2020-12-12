module Main where

import Control.Arrow ((&&&))

import qualified Data.Map as M
import Data.Map (Map)

data Usage = Floor | Seat | Occupied deriving (Eq, Show)
type Coord = (Int, Int)
type Layout = Map Coord Usage
type Input = Layout

neighbors :: Coord -> [Coord]
neighbors c = map (add c) . tail $ do
  dy <- [0,1,-1]
  dx <- [0,1,-1]
  pure (dy, dx)
  where add (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

nextState :: Layout -> Coord -> Usage -> Usage
nextState m k v = case v of
  Floor -> Floor
  Seat | liveNeighbors == 0 -> Occupied
       | otherwise -> Seat
  Occupied | liveNeighbors >= 4 -> Seat
           | otherwise -> Occupied

  where liveNeighbors = length . filter (== Occupied)
                        . map (flip (M.findWithDefault Floor) m) . neighbors
                        $ k

tick :: Layout -> Layout
tick m = M.mapWithKey (nextState m) m

part1 :: Input -> Int
part1 m = let states = iterate tick m
              pairs = zip <*> tail $ states
              firstDup = fst . head . dropWhile (uncurry (/=)) $ pairs
          in length . filter (== Occupied) . M.elems $ firstDup

part2 :: Input -> ()
part2 = const ()

prepare :: String -> Input
prepare input = M.fromList $ do
  (y, row) <- zip [0..] $ lines input
  (x, cell) <- zip [0..] row
  pure ((y, x), parse cell)
  where parse '.' = Floor
        parse 'L' = Seat

main :: IO ()
main = readFile "input.txt" >>= print . (part1 &&& part2) . prepare
