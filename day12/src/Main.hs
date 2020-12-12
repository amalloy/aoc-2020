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

follow :: Ship -> Instruction -> Ship
follow (Ship h pos) (Instruction t n) = case t of
  Absolute c -> Ship h (move n c pos)
  Relative F -> Ship h (move n h pos)
  Relative r -> Ship h' pos
    where h' = iterate (turn r) h !! (n `div` 90)

manhattan :: Coord -> Int
manhattan (x, y) = abs x + abs y

part1 :: Input -> Int
part1 = manhattan . position . foldl' follow (Ship E (0, 0))

part2 :: Input -> ()
part2 = const ()

prepare :: String -> Input
prepare = map parse . lines
  where parse (c:n) = Instruction d (read n)
          where d = case c of
                      'N' -> Absolute N
                      'S' -> Absolute S
                      'E' -> Absolute E
                      'W' -> Absolute W
                      'F' -> Relative F
                      'R' -> Relative R
                      'L' -> Relative L

main :: IO ()
main = readFile "input.txt" >>= print . (part1 &&& part2) . prepare
