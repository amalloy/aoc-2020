{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE LambdaCase #-}
module Main where

import Control.Arrow ((&&&))
import Data.Bool (bool)

type Input = [String]

asBinary :: (a -> Bool) -> [a] -> Int
asBinary one = foldl (\n x -> 2 * n + digit x) 0
  where digit = bool 0 1 . one

part1 :: Input -> Int
part1 = maximum . map (asBinary (`elem` "BR"))

part2 :: Input -> ()
part2 i = ()

prepare :: String -> Input
prepare = lines

main :: IO ()
main = readFile "input.txt" >>= print . (part1 &&& part2) . prepare
