module Main where

import Control.Arrow ((&&&))
import Data.Foldable (toList)
import Data.List.Split (splitOn)
import Data.Maybe (mapMaybe)

import Data.Sequence (Seq((:<|)), (|>))
import qualified Data.Sequence as Seq
import Data.Set (Set)
import qualified Data.Set as Set

import Control.Monad.Trans.State.Strict (State, evalState, gets, modify)

type Card = Int
type Deck = Seq Card

type Input = (Deck, Deck)

p1WinsGame :: (Deck, Deck) -> State (Set (Deck, Deck)) (Bool, Deck)
p1WinsGame (Seq.Empty, p2) = pure (False, p2)
p1WinsGame (p1, Seq.Empty) = pure (True, p1)
p1WinsGame g@(d1@(c1 :<| p1), (c2 :<| p2)) = do
  loop <- gets (Set.member g)
  if loop
    then pure (True, d1)
    else do
      modify $ Set.insert g
      let p1' = Seq.take c1 p1
          p2' = Seq.take c2 p2
          roundWinner = if Seq.length p1' == c1 && Seq.length p2' == c2
            then fst $ evalState (p1WinsGame (p1', p2')) Set.empty
            else c1 > c2
      if roundWinner
        then p1WinsGame (p1 |> c1 |> c2, p2)
        else p1WinsGame (p1, p2 |> c2 |> c1)

score :: Deck -> Int
score = sum . zipWith (*) [1..] . toList . Seq.reverse

part1 :: Input -> Int
part1 decks = let winner (Seq.Empty, p2) = Just p2
                  winner (p1, Seq.Empty) = Just p1
                  winner _ = Nothing
                  stages = iterate oneRound decks
                  final = head . mapMaybe winner $ stages
              in score final
  where oneRound (c1 :<| p1, c2 :<| p2) | c1 > c2 = (p1 |> c1 |> c2, p2)
                                        | c1 < c2 = (p1, p2 |> c2 |> c1)


part2 :: Input -> Int
part2 decks =
  let (_, winner) = evalState (p1WinsGame decks) Set.empty
  in score winner

prepare :: String -> Input
prepare = deal . map parse . splitOn [""] . lines
  where deal [a, b] = (a, b)
        parse (playerName : cards) = Seq.fromList $ map read cards

main :: IO ()
main = readFile "input.txt" >>= print . (part1 &&& part2) . prepare
