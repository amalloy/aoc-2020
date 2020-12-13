module Main where

import Control.Arrow ((&&&))
import Data.List (mapAccumL, minimumBy)
import Data.List.Split (splitOn)
import Data.Maybe (catMaybes)
import Data.Ord (comparing)

data Bus = Operating Int | X
data Constraint = Constraint {residue, prime :: Int} deriving Show

type Input = (Int, [Bus])

part1 :: Input -> Int
part1 (start, buses) =
  uncurry (*) . minimumBy (comparing snd) $
  [(bus, nextTime bus) | Operating bus <- buses]
  where nextTime bus = bus - (start `mod` bus)

part2 :: Input -> Int
part2 = residue . foldl1 combine .
  catMaybes . snd . mapAccumL parseConstraint 0 . snd
  where parseConstraint :: Int -> Bus -> (Int, Maybe Constraint)
        parseConstraint offset bus = (succ offset, case bus of
          X -> Nothing
          Operating p -> Just $ Constraint r p
            where r = (p - offset) `mod` p)

combine :: Constraint -> Constraint -> Constraint
combine (Constraint n p) (Constraint m q) =
  Constraint x (p * q)
  where x = head . filter ((== m) . (`mod` q)) . iterate (+ p) $ n

prepare :: String -> Input
prepare = parse . lines
  where parse [start, schedule] = (read start, map bus . splitOn "," $ schedule)
        bus "x" = X
        bus n | prime b = Operating b
              | otherwise = error (show b)
          where b = read n
        prime n = null [m | m <- [2..n-1], n `mod` m == 0]

main :: IO ()
main = readFile "input.txt" >>= print . (part1 &&& part2) . prepare
