module Main where

import Control.Arrow ((&&&))

import qualified Data.Map as M
import Data.Map (Map)

data Usage = Floor | Seat | Occupied deriving (Eq, Show)
type Coord = (Int, Int)
type Layout = Map Coord Usage
type Input = Layout

type Ticker = Layout -> Coord -> [Usage]
type Pickiness = Int
data Parameters = Parameters {pickiness :: Int, step :: Ticker}

deltas :: [Coord]
deltas = tail $ do
  dy <- [0,1,-1]
  dx <- [0,1,-1]
  pure (dy, dx)

add :: Coord -> Coord -> Coord
add (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

neighbors :: Ticker
neighbors m c = map (flip (M.findWithDefault Seat) m . add c) deltas

transition :: Int -> (Int -> Usage -> Usage)
transition p liveNeighbors v = case v of
  Floor -> Floor
  Seat | liveNeighbors == 0 -> Occupied
       | otherwise -> Seat
  Occupied | liveNeighbors >= p -> Seat
           | otherwise -> Occupied

nextState :: Parameters -> Layout -> Coord -> Usage -> Usage
nextState (Parameters p t) m k = transition p liveNeighbors
  where liveNeighbors = length . filter (== Occupied) . t m $ k

tick :: Parameters -> Layout -> Layout
tick p m = M.mapWithKey (nextState p m) m

solve :: Parameters -> Input -> Int
solve p m = let states = iterate (tick p) m
                pairs = zip <*> tail $ states
                firstDup = fst . head . dropWhile (uncurry (/=)) $ pairs
            in length . filter (== Occupied) . M.elems $ firstDup

part1 :: Input -> Int
part1 = solve (Parameters 4 neighbors)

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
