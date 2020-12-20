{-# LANGUAGE LambdaCase #-}

module Main where

import Control.Arrow ((&&&))
import Data.Char (isDigit)
import Data.Foldable (asum)
import Data.List (foldl')
import Data.Maybe (fromJust)

import Text.Regex.Applicative

import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as I
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M

data Orientation = Orientation {cwTurns :: Int, flipped :: Bool}
data Piece = Piece Int [String] deriving Show

canonicalize :: String -> Int
canonicalize s = max (asInt s) (asInt $ reverse s)
  where asInt = foldl' f 0 . map (\case '.' -> 0; '#' -> 1)
        f acc x = 2 * acc + x

edges :: [String] -> [String]
edges = sequence [head, last, map head, map last]

ints :: [String] -> [Int]
ints strs = canonicalize <$> edges strs

type Input = [Piece]

frequencies :: [Int] -> IntMap Int
frequencies = I.fromListWith (+) . flip zip (repeat 1)

-- see how often each edge is represented
explore :: Input -> IntMap Int
explore = frequencies . I.elems . frequencies . (>>= asEdges)

asEdges :: Piece -> [Int]
asEdges (Piece _ img) = map canonicalize $ edges img

part1 :: Input -> Int
part1 ps = let freqs = frequencies . (>>= asEdges) $ ps
               isCorner img = let es = map canonicalize . edges $ img
                              in length (filter ((== 1) . (freqs I.!)) es) == 2
           in product [id | Piece id img <- ps, isCorner img]

assemble :: Input -> Map (Int, Int) (Piece, Orientation)
assemble ps = m
  where freqs = frequencies . (>>= asEdges) $ ps
        isCorner img = let es = map canonicalize . edges $ img
                       in length (filter ((== 1) . (freqs I.!)) es) == 2
        topLeft = head [p | p@(Piece _ img) <- ps, isCorner img]

part2 :: Input -> ()
part2 = const ()

prepare :: String -> Input
prepare = fromJust . (=~ input)
  where input = many piece
        piece = Piece <$> tileId <*> many line <* sym '\n'
        tileId = string "Tile " *> int <* string ":\n"
        line = many (asum (map sym ".#")) <* sym '\n'
        int = read <$> many (psym isDigit)

main :: IO ()
main = readFile "input.txt" >>= print . (explore &&& part1 &&& part2) . prepare
