{-# LANGUAGE LambdaCase #-}

module Main where

import Control.Arrow ((&&&))
import Data.List (foldl')

type Coord = (Int, Int)
data Instruction = Instruction Heading Int deriving Show
data Heading = Absolute Compass | Relative Relative deriving Show
data Compass = N | E | S | W deriving Show
data Relative = F | L | R deriving Show
data Ship = Ship {heading :: Compass, position :: Coord}
data Traveler = Traveler {ship, waypoint :: Coord} deriving Show

type Input = [Instruction]

toDelta :: Compass -> Coord
toDelta = \case
  N -> (0, 1)
  S -> (0, -1)
  W -> (-1, 0)
  E -> (1, 0)

turn :: Relative -> Compass -> Compass
turn F c = c
turn L c = case c of
  N -> W
  W -> S
  S -> E
  E -> N
turn R c = l . l . l $ c
  where l = turn L

move :: Int -> Compass -> Coord -> Coord
move n d (x, y) = let (dx, dy) = toDelta d
                  in (x + dx * n, y + dy * n)


manhattan :: Coord -> Int
manhattan (x, y) = abs x + abs y

part1 :: Input -> Int
part1 = manhattan . position . foldl' follow (Ship E (0, 0))
  where follow :: Ship -> Instruction -> Ship
        follow (Ship h pos) (Instruction t n) = case t of
          Absolute c -> Ship h (move n c pos)
          Relative F -> Ship h (move n h pos)
          Relative r -> Ship h' pos
            where h' = iterate (turn r) h !! n


part2 :: Input -> Int
part2 = manhattan . ship .foldl' useWaypoint (Traveler (0, 0) (10, 1))

useWaypoint :: Traveler -> Instruction -> Traveler
useWaypoint (Traveler pos wp) (Instruction t n) = case t of
  Absolute c -> Traveler pos (move n c wp)
  Relative F -> Traveler pos' wp
    where pos' = let (x, y) = pos
                     (dx, dy) = wp
                 in (x + n * dx, y + n * dy)
  Relative R -> Traveler pos (right 1)
  Relative L -> Traveler pos (right 3)
  where right m = iterate turnRight wp !! (n * m)
        turnRight (x, y) = (y, -x)


prepare :: String -> Input
prepare = map parse . lines
  where parse (c:n) = case c of
                        'N' -> raw N
                        'S' -> raw S
                        'E' -> raw E
                        'W' -> raw W
                        'F' -> Instruction (Relative F) amt
                        'R' -> scaled R
                        'L' -> scaled L
          where amt = read n
                raw d = Instruction (Absolute d) amt
                scaled d = Instruction (Relative d) (amt `div` 90)

main :: IO ()
main = readFile "input.txt" >>= print . (part1 &&& part2) . prepare
