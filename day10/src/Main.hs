module Main where

import Control.Arrow ((&&&))
import Data.List (group, sort)

type Input = [Int]

part1 :: Input -> Int
part1 = product . map length . group . sort
  . (3:) . filter (`elem` [1,3])
  . (zipWith (flip (-)) <*> tail) . sort . (0:)

part2 :: Input -> ()
part2 = const ()

prepare :: String -> Input
prepare = map read . lines

main :: IO ()
main = readFile "input.txt" >>= print . (part1 &&& part2) . prepare
