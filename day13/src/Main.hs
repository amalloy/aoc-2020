module Main where

import Control.Arrow ((&&&))
import Data.List (minimumBy)
import Data.List.Split (splitOn)
import Data.Ord (comparing)

data Bus = Operating Int | X

type Input = (Int, [Bus])

part1 :: Input -> Int
part1 (start, buses) =
  uncurry (*) . minimumBy (comparing snd) $
  [(bus, nextTime bus) | Operating bus <- buses]
  where nextTime bus = bus - (start `mod` bus)

part2 :: Input -> ()
part2 = const ()

prepare :: String -> Input
prepare = parse . lines
  where parse [start, schedule] = (read start, map bus . splitOn "," $ schedule)
        bus "x" = X
        bus n = Operating $ read n

main :: IO ()
main = readFile "input.txt" >>= print . (part1 &&& part2) . prepare
