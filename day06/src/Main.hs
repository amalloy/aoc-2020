module Main where

import Control.Arrow ((&&&))
import qualified Data.Set as S
import Data.List (foldl1')
import Data.List.Split (splitOn)

type Input = [[S.Set Char]]

sumGroupsVia :: Ord a => (S.Set a -> S.Set a -> S.Set a) -> [[S.Set a]] -> Int
sumGroupsVia f = sum . map (length . foldl1' f)

part1 :: Input -> Int
part1 = sumGroupsVia S.union

part2 :: Input -> Int
part2 = sumGroupsVia S.intersection

prepare :: String -> Input
prepare = map (map S.fromList . lines) . splitOn "\n\n" . init

main :: IO ()
main = readFile "input.txt" >>= print . (part1 &&& part2) . prepare
