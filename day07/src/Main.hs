{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE LambdaCase #-}

module Main where

import Control.Arrow ((&&&))

import qualified Data.Map.Strict as M
import qualified Data.Set as S

import Data.Char (isDigit)
import Data.Maybe (fromMaybe)
import Text.Regex.Applicative

data Color = Color String String deriving (Eq, Ord, Show)
data Edge = Edge Color Int deriving Show
type Graph = M.Map Color [Edge]
type Input = Graph

part1 :: Input -> ()
part1 = const ()

part2 :: Input -> ()
part2 = const ()


prepare :: String -> Input
prepare = M.fromList . fromMaybe [] . traverse parse . lines

parse :: String -> Maybe (Color, [Edge])
parse = (=~ rule)
  where
    rule :: RE Char (Color, [Edge])
    rule = do
      outer <- bag
      string "s contain "
      contents <- empty <|> full
      pure (outer, contents)
        where full = many $
                  uncurry Edge <$>
                  numbered bag <* (string ", " <|> string ".")
              empty = [] <$ string "no other bags."
    bag :: RE Char Color
    bag = do
      descriptor <- word
      space
      base <- word
      space
      string "bag"
      pure $ Color descriptor base
    numbered :: RE Char a -> RE Char (a, Int)
    numbered p = do
      num <- read <$> many (psym isDigit)
      space
      x <- p
      optional $ sym 's'
      pure (x, num)
    space = sym ' '
    word = many (psym (/= ' '))


main :: IO ()
main = readFile "input.txt" >>= print . (part1 &&& part2) . prepare
