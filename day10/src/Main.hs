module Main where

import Control.Arrow ((&&&))
import Data.List (group, sort)

import Data.IntMap.Lazy (IntMap)
import qualified Data.IntMap.Lazy as M
import qualified Data.Set as S

type Input = [Int]

part1 :: Input -> Int
part1 = product . map length . group . sort
  . (3:) . filter (`elem` [1,3])
  . (zipWith (flip (-)) <*> tail) . sort . (0:)

part2 :: Input -> Int
part2 adapters = let inputs = S.fromList adapters
                     goal = S.findMax inputs + 3
                     available = S.insert goal inputs
                     table :: IntMap Int
                     table = M.fromList [(x, solve x) | x <- [-2..goal]]
                     solve :: Int -> Int
                     solve 0 = 1
                     solve x | S.member x available = sum $ map (table M.!) [x-3, x-2, x-1]
                             | otherwise = 0
  in table M.! goal

prepare :: String -> Input
prepare = map read . lines

main :: IO ()
main = readFile "input.txt" >>= print . (part1 &&& part2) . prepare
