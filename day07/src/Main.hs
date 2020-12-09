{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE LambdaCase #-}

module Main where

import Control.Arrow ((&&&))

import qualified Data.Map.Lazy as M
import qualified Data.Set as S

import Data.Char (isDigit)
import Data.Maybe (fromMaybe)
import Text.Regex.Applicative

data Color = Color String String deriving (Eq, Ord, Show)
data Edge = Edge Color Int deriving Show
type Graph = M.Map Color [Edge]
type Input = Graph

fixMap :: (Color -> [(Int, a)] -> a) -> Graph -> M.Map Color a
fixMap f g = table
  where table = M.fromList $ do
          (color, edges) <- M.assocs g
          pure (color, f color . map fixEdge $ edges)
        fixEdge (Edge color weight) = (weight, table M.! color)

canReach :: Color -> Graph -> S.Set Color
canReach goal =
  S.delete goal . S.fromList
  . map fst . filter (S.member goal . snd)
  . M.assocs . fixMap reachable
  where reachable color = S.insert color . S.unions . map snd

gold :: Color
gold = Color "shiny" "gold"

weightOf :: Color -> Graph -> Int
weightOf root = (M.! root) . fixMap weight
  where weight _color = succ . sum . map (uncurry (*))

part1 :: Input -> Int
part1 = length . canReach gold

part2 :: Input -> Int
part2 = pred . weightOf gold

prepare :: String -> Input
prepare = M.fromList . fromMaybe [] . traverse parse . lines

parse :: String -> Maybe (Color, [Edge])
parse = (=~ rule)
  where
    rule :: RE Char (Color, [Edge])
    rule = do
      outer <- bag
      string "s contain "
      contents <- leaf <|> node
      pure (outer, contents)
      where leaf = [] <$ string "no other bags."
            node = many $
                uncurry Edge <$>
                numbered bag <* (string ", " <|> string ".")

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
