module Main where

import Control.Arrow ((&&&))
import Data.Char (digitToInt, intToDigit)

import Data.Sequence (Seq)
import qualified Data.Sequence as S

newtype Label = Label {getLabel :: Int} deriving (Eq, Show)
data GameState = GameState (Seq Label) Label deriving Show

type Input = GameState

numCupsMoved :: Int
numCupsMoved = 3

advance :: GameState -> GameState
advance (GameState cups label) =
  let Just idx = S.elemIndexL label cups
      size = S.length cups
      (middle', right) = S.splitAt (idx + 1) cups
      (pickedUpR, remainder) = S.splitAt numCupsMoved right
      lSize = numCupsMoved - S.length pickedUpR
      (pickedUpL, kept) = S.splitAt lSize (middle' <> remainder)
      pickedUp = pickedUpR <> pickedUpL
      destinationLabel = head . filter onTable . tail . iterate dec $ label
        where dec (Label x) = Label ((x - 1) `mod` (size + 1))
              onTable = (`elem` kept)
      Just destinationIndex = S.elemIndexL destinationLabel kept
      (before, after) = S.splitAt (destinationIndex + 1) kept
      cups' = mconcat [before, pickedUp, after]
      Just idx' = S.elemIndexL label cups'
  in GameState cups' (S.index cups' ((idx' + 1)`mod` size))

part1 :: Input -> String
part1 state = let (GameState final _) = iterate advance state !! 100
                  Just oneIdx = S.elemIndexL (Label 1) final
                  atPosition i = S.index final $ (i + oneIdx) `mod` S.length final
              in map (intToDigit . getLabel . atPosition) $ [1..S.length final - 1]

part2 :: Input -> ()
part2 = const ()

prepare :: String -> Input
prepare s = GameState cs first
  where cs = S.fromList digits
        digits = map (Label . digitToInt) . filter (/= '\n') $ s
        first = head digits

main :: IO ()
main = readFile "input.txt" >>= print . (part1 &&& part2) . prepare
