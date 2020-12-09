{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE LambdaCase #-}

module Main where

import Control.Arrow ((&&&))

import Data.Sequence (Seq((:<|)), (|>))
import Data.IntMap.Strict (IntMap)
import qualified Data.Sequence as S
import qualified Data.IntMap.Strict as M

type Input = (State, [Int])

data State = State (IntMap Int) (Seq Int)

consumeNumber :: Int -> State -> State
consumeNumber x (State m (oldest :<| others)) = State m' q'
  where q' = others |> x
        m' = M.insertWith (+) x 1 . M.update decrease oldest $ m
        decrease 0 = Nothing
        decrease n = Just (pred n)

valid :: State -> Int -> Bool
valid (State m _) x = any hasPair (M.keys m)
  where hasPair y = M.member (x - y) m

part1 :: Input -> Int
part1 (_, []) = error "whole message valid"
part1 (state, (x:xs)) | valid state x = part1 ((consumeNumber x state), xs)
                      | otherwise = x

part2 :: Input -> ()
part2 = const ()

prepare :: String -> Input
prepare input =
  let nums = map read . lines $ input
      (preamble, text) = splitAt 25 nums
      bag = M.fromListWith (+) [(x, 1) | x <- preamble]
      queue = S.fromList preamble
  in (State bag queue, text)

main :: IO ()
main = readFile "input.txt" >>= print . (part1 &&& part2) . prepare
