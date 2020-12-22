module Main where

import Control.Arrow ((&&&))
import Data.Foldable (toList)
import Data.List.Split (splitOn)
import Data.Maybe (mapMaybe)

import Data.Sequence (Seq((:<|)), (|>))
import qualified Data.Sequence as S

type Card = Int
type Deck = Seq Card

type Input = (Deck, Deck)

oneRound :: (Deck, Deck) -> (Deck, Deck)
oneRound (c1 :<| p1, c2 :<| p2) | c1 > c2 = (p1 |> c1 |> c2, p2)
                                | c1 < c2 = (p1, p2 |> c2 |> c1)

part1 :: Input -> Int
part1 decks = let winner (S.Empty, p2) = Just p2
                  winner (p1, S.Empty) = Just p1
                  winner _ = Nothing
                  stages = iterate oneRound decks
                  final = head . mapMaybe winner $ stages
                  score = sum . zipWith (*) [1..] . toList . S.reverse
              in score final


part2 :: Input -> ()
part2 = const ()

prepare :: String -> Input
prepare = deal . map parse . splitOn [""] . lines
  where deal [a, b] = (a, b)
        parse (playerName : cards) = S.fromList $ map read cards

main :: IO ()
main = readFile "input.txt" >>= print . (part1 &&& part2) . prepare
