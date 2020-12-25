module Main where

import Control.Arrow ((&&&))

import Data.Semigroup (stimes)

type Input = (Int, Int)

modulo, initialSubject :: Int
modulo = 20201227
initialSubject = 7

newtype Exponentiation = Exponentiation Int

instance Semigroup Exponentiation where
  (Exponentiation a) <> (Exponentiation b) = Exponentiation $ (a * b) `mod` modulo

findLoopSize :: Int -> Int
findLoopSize publicKey =
  length . takeWhile (/= publicKey) . iterate transform $ 1
  where transform n = (n * initialSubject) `mod` modulo

part1 :: Input -> Int
part1 (door, card) = let doorLoopSize = findLoopSize door
                         (Exponentiation x) = stimes doorLoopSize (Exponentiation card)
                     in x

part2 :: Input -> ()
part2 = const ()

prepare :: String -> Input
prepare = tuple . map read . lines
  where tuple [x, y] = (x, y)

main :: IO ()
main = readFile "input.txt" >>= print . (part1 &&& part2) . prepare
