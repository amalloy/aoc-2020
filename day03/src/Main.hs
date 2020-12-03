module Main where

import Control.Arrow ((&&&))

import Data.Set (Set)
import qualified Data.Set as S

type Input = (Int, [Set Int])
data Slope = Slope {right, down :: Int}

takeNth :: Int -> [a] -> [a]
takeNth _ [] = []
takeNth n xs@(x:_) = x : takeNth n (drop n xs)

hitTree :: Int -> Int -> Set Int -> Bool
hitTree width x = S.member (x `mod` width)

treeCount :: Input -> Slope -> Int
treeCount (width, forest) (Slope r d) =
  length
  . filter (uncurry (hitTree width))
  . zip [0, r..]
  . takeNth d
  $ forest

part1 :: Input -> Int
part1 = (`treeCount` (Slope 3 1))


part2 :: Input -> Int
part2 i = product . map (treeCount i) $ [ Slope 1 1
                                        , Slope 3 1
                                        , Slope 5 1
                                        , Slope 7 1
                                        , Slope 1 2
                                        ]

prepare :: String -> Input
prepare = (length . head &&& map readTrees) . lines
  where readTrees :: String -> Set Int
        readTrees = S.fromList . map fst . filter ((== '#') . snd) . zip [0..]

main :: IO ()
main = readFile "input.txt" >>= print . (part1 &&& part2) . prepare
