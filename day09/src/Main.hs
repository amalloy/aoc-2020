{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE LambdaCase #-}

module Main where

import Control.Arrow ((&&&))
import Control.Monad (guard)

import Data.Sequence (Seq((:<|)), (|>))
import Data.IntMap.Strict (IntMap)
import qualified Data.Sequence as S
import qualified Data.IntMap.Strict as I
import qualified Data.Map.Lazy as M

type Input = (State, [Int])

data State = State (IntMap Int) (Seq Int)

consumeNumber :: Int -> State -> State
consumeNumber x (State m (oldest :<| others)) = State m' q'
  where q' = others |> x
        m' = I.insertWith (+) x 1 . I.update decrease oldest $ m
        decrease 0 = Nothing
        decrease n = Just (pred n)

valid :: State -> Int -> Bool
valid (State m _) x = any hasPair (I.keys m)
  where hasPair y = I.member (x - y) m

rangeSums :: Seq Int -> M.Map (Int, Int) Int
rangeSums q = table
  where table = M.fromList $ do
          lo <- [0..S.length q - 1]
          pure ((lo, lo), (S.index q lo)) ++ do
            hi <- [lo+1..S.length q - 1]
            pure ((lo, hi), S.index q hi + (table M.! (lo, hi-1)))

part1 :: Input -> Int
part1 (_, []) = error "whole message valid"
part1 (state, (x:xs)) | valid state x = part1 ((consumeNumber x state), xs)
                      | otherwise = x

part2 :: Input -> [Int]
part2 i@(State _ preamble, text) = do
  ((lo, hi), total) <- M.assocs $ rangeSums q
  guard $ lo /= hi
  guard $ total == goal
  let range = S.take (hi - lo + 1) . S.drop lo $ q
  pure $ minimum range + maximum range
  where goal = part1 i
        q = preamble S.>< S.fromList text

prepare :: String -> Input
prepare input =
  let nums = map read . lines $ input
      (preamble, text) = splitAt 25 nums
      bag = I.fromListWith (+) [(x, 1) | x <- preamble]
      queue = S.fromList preamble
  in (State bag queue, text)

main :: IO ()
main = readFile "input.txt" >>= print . (part1 &&& part2) . prepare
