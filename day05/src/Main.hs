module Main where

import Control.Arrow ((&&&))
import Data.Bool (bool)
import Data.List (foldl', sort)

type Input = [Int]

asBinary :: (a -> Bool) -> [a] -> Int
asBinary one = foldl' (\n x -> 2 * n + digit x) 0
  where digit = bool 0 1 . one

part1 :: Input -> Int
part1 = maximum

part2 :: Input -> [Int]
part2 = map (succ . fst) . filter (\(x, y) -> y - x == 2) . (zip <*> tail) . sort

prepare :: String -> Input
prepare = map (asBinary (`elem` "BR")) . lines

main :: IO ()
main = readFile "input.txt" >>= print . (part1 &&& part2) . prepare
