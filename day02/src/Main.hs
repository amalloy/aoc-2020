module Main where

import Control.Arrow ((&&&))
import Data.Char (isDigit)
import Data.Maybe (fromMaybe)

import Text.Regex.Applicative

data Rule = Rule {atLeast, atMost :: Int, char :: Char} deriving Show
data Password = Password Rule String deriving Show
type Input = [Password]

part1 :: Input -> Int
part1 = length . filter valid
  where valid (Password (Rule lo hi c) p) = numOccurs >= lo && numOccurs <= hi
          where numOccurs = length $ filter (== c) p

part2 :: Input -> ()
part2 i = ()

prepare :: String -> Input
prepare = fromMaybe [] . traverse parse . lines
  where parse = (=~ password)
        password = Password <$> rule <*> many anySym
        rule = Rule <$> int <* sym '-' <*> int <* sym ' ' <*> anySym <* string ": "
        int = read <$> many (psym isDigit)

main :: IO ()
main = readFile "input.txt" >>= print . (part1 &&& part2) . prepare
