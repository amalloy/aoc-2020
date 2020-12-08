{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE LambdaCase #-}

module Main where

import Control.Arrow ((&&&))

import qualified Data.Map.Strict as M
import qualified Data.Set as S

import Control.Monad (unless)
import Control.Monad.Trans.State.Lazy (evalState, execState, State, modify, gets)

import Data.Char (isDigit)
import Data.Foldable (traverse_)
import Data.Maybe (fromMaybe)
import Text.Regex.Applicative

data Color = Color String String deriving (Eq, Ord, Show)
data Edge = Edge Color Int deriving Show
type Graph = M.Map Color [Edge]
type Input = Graph

reachableFrom :: Color -> Graph -> S.Set Color
reachableFrom root g = flip execState S.empty $ walk root
  where containedIn :: M.Map Color (S.Set Color)
        containedIn = M.fromListWith (<>) $ do
          (owner, contents) <- M.assocs g
          Edge inner _num <- contents
          pure (inner, S.singleton owner)
        walk :: Color -> State (S.Set Color) ()
        walk c = do
          seen <- gets (S.member c)
          unless seen $ do
            modify (S.insert c)
            traverse_ walk $ M.findWithDefault S.empty c containedIn

gold :: Color
gold = Color "shiny" "gold"

weightOf :: Color -> Graph -> State (M.Map Color Int) Int
weightOf root g = gets (M.lookup root) >>=
  \case
    Just w -> pure w
    Nothing -> do
      weight <- succ . sum <$> traverse weigh (g M.! root)
      modify (M.insert root weight)
      pure weight
  where weigh :: Edge -> State (M.Map Color Int) Int
        weigh (Edge c num) = (num *) <$> weightOf c g

part1 :: Input -> Int
part1 = length . S.delete gold . reachableFrom gold

part2 :: Input -> Int
part2 = pred . flip evalState M.empty . weightOf gold

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
