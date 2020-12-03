module Main where

import Control.Arrow ((&&&))
import Data.Char (isDigit)
import Data.Maybe (fromMaybe)

import Text.Regex.Applicative

data Rule = Rule {lo, hi :: Int, char :: Char} deriving Show
data Password = Password Rule String deriving Show
type Input = [Password]

numValid :: (a -> Bool) -> [a] -> Int
numValid p = length . filter p

part1 :: Input -> Int
part1 = numValid valid
  where valid (Password (Rule lo hi c) p) = numOccurs >= lo && numOccurs <= hi
          where numOccurs = numValid (== c) p

part2 :: Input -> Int
part2 = numValid valid
  where valid (Password (Rule lo hi c) p) =
          (== 1) . numValid (== c)
          . map ((p !!) . pred) $ [lo, hi]

prepare :: String -> Input
prepare = fromMaybe [] . traverse parse . lines
  where parse = (=~ password)
        password = Password <$> rule <*> many anySym
        rule = Rule <$> int <* sym '-' <*> int <* sym ' ' <*> anySym <* string ": "
        int = read <$> many (psym isDigit)

main :: IO ()
main = readFile "input.txt" >>= print . (part1 &&& part2) . prepare
