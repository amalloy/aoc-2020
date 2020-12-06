module Main where

import Control.Arrow ((&&&))
import qualified Data.Set as S
import Data.List (foldl')
import Data.List.Split (splitOn)

type Input = [[S.Set Char]]

part1 :: Input -> Int
part1 = sum . map (length . foldl' S.union S.empty)

part2 :: Input -> ()
part2 = const ()

prepare :: String -> Input
prepare = map (map S.fromList . lines) . splitOn "\n\n" . init

main :: IO ()
main = readFile "input.txt" >>= print . (part1 &&& part2) . prepare
