{-# LANGUAGE LambdaCase #-}

module Main where

import Control.Arrow ((&&&))
import Control.Monad (replicateM)
import Data.Char (isDigit)
import Data.Foldable (asum)
import Data.List.Split (splitOn)
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe, isJust)
import Text.Regex.Applicative

type Passport = M.Map String String
type Input = [Passport]
type Rule = String -> Bool

data Height = Cm Int | In Int deriving Show

rule :: RE Char a -> (a -> Bool) -> Rule
rule re valid = maybe False valid . (=~ re)

between :: Int -> Int -> (Int -> Bool)
between lo hi x = x >= lo && x <= hi

int :: RE Char Int
int = read <$> many (psym isDigit)

height :: RE Char Height
height = Cm <$> (int <* string "cm")
         <|> In <$> (int <* string "in")

regex :: RE Char a -> Rule
regex re = isJust . (=~ re)

rules :: M.Map String Rule
rules = M.fromList [ ("byr", rule int (between 1920 2002))
                   , ("iyr", rule int (between 2010 2020))
                   , ("eyr", rule int (between 2020 2030))
                   , ("hgt", rule height (\case Cm c -> between 150 193 c
                                                In i -> between 59 76 i))
                   , ("hcl", regex (sym '#' *> (replicateM 6 . asum $ map sym
                                                "0123456789abcdef")))
                   , ("ecl", regex (asum $ map string
                                    ["amb", "blu", "brn", "gry", "grn", "hzl", "oth"]))
                   , ("pid", regex (replicateM 9 (psym isDigit)))
                   ]

hasAllFields :: Passport -> Bool
hasAllFields = (== 7) . length . M.keys

part1 :: Input -> Int
part1 = length . filter hasAllFields

part2 :: Input -> Int
part2 = length . filter validFieldValues . filter hasAllFields
  where validFieldValues = all checkField . M.toList
        checkField (name, value) = M.findWithDefault (const False) name rules value

prepare :: String -> Input
prepare = fromMaybe [] . traverse passport . splitOn "\n\n"
  where passport = fmap (M.delete "cid" . M.fromList) . (=~ r)
        r :: RE Char [(String, String)]
        r = many field
        field :: RE Char (String, String)
        field = (,) <$>
          (many (psym whitespace)
          *> many (psym (/= ':')) :: RE Char String)
          <*> (sym ':'
          *> many (psym (not . whitespace)) :: RE Char String)
          <* many (psym whitespace)
        whitespace = (`elem` " \n")

main :: IO ()
main = readFile "input.txt" >>= print . (part1 &&& part2) . prepare
