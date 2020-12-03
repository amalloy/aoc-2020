module Main where

import Control.Arrow ((&&&))

import Data.Set (Set)
import qualified Data.Set as S

type Input = (Int, [Set Int])

hitTree :: Int -> Int -> Set Int -> Bool
hitTree width x = S.member (x `mod` width)

part1 :: Input -> Int
part1 (width, trees) =
  length
  . filter (uncurry (hitTree width))
  . zip [0, 3..]
  $ trees

part2 :: Input -> ()
part2 i = ()

prepare :: String -> Input
prepare = (length . head &&& map readTrees) . lines
  where readTrees :: String -> Set Int
        readTrees = S.fromList . map fst . filter ((== '#') . snd) . zip [0..]

main :: IO ()
main = readFile "input.txt" >>= print . (part1 &&& part2) . prepare
