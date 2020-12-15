module Main where

import Control.Arrow ((&&&))
import Data.List.Split (splitOn)

import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as M

type Input = [Int]

data GameState = GameState {turnCount, mostRecent :: Int, spokenMap :: IntMap Int}
  deriving Show

step :: GameState -> GameState
step (GameState turnCount mostRecent spokenMap) =
  GameState (succ turnCount) next $ M.insert mostRecent turnCount spokenMap
  where next :: Int
        next = case M.lookup mostRecent spokenMap of
          Nothing -> 0
          Just previous -> turnCount - previous

initialize :: Input -> GameState
initialize inputs =
  GameState { turnCount = length inputs
            , mostRecent = last inputs
            , spokenMap = M.fromList . flip zip [1..] . init $ inputs
            }

solve :: Int -> Input -> Int
solve n = mostRecent . head . filter ((== n) . turnCount) . iterate step . initialize

part1 :: Input -> Int
part1 = solve 2020

part2 :: Input -> Int
part2 = solve 30000000

prepare :: String -> Input
prepare = map read . splitOn ","

main :: IO ()
main = readFile "input.txt" >>= print . (part1 &&& part2) . prepare
