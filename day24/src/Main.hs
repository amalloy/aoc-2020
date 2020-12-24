{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Main where

import Control.Arrow ((&&&))
import Data.Foldable (asum)
import Data.Monoid (Sum)
import Data.Maybe (fromMaybe)

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Text.Regex.Applicative

newtype AxialCoord = AxialCoord (Sum Int, Sum Int)
  deriving (Ord, Eq, Monoid, Semigroup, Show)

data Color = Black | White deriving Show

black :: Color -> Bool
black Black = True
black White = False

west, east, northWest, southEast, northEast, southWest :: AxialCoord
west = AxialCoord (-1, 0)
east = AxialCoord (1, 0)
northWest = AxialCoord (0, -1)
southEast = AxialCoord (0, 1)
northEast = northWest <> east
southWest = southEast <> west

type Input = [AxialCoord]

seed :: Input -> Map AxialCoord Color
seed = M.fromListWith combine . map start
  where start coord = (coord, Black)
        combine Black Black = White
        combine White White = White
        combine _ _ = Black

part1 :: Input -> Int
part1 = length . filter black . M.elems . seed

part2 :: Input -> ()
part2 = const ()

prepare :: String -> Input
prepare = map mconcat . fromMaybe [] . traverse parse . lines
  where parse = (=~ path)
        path = many coord :: RE Char [AxialCoord]
        coord :: RE Char AxialCoord
        coord = asum [ dir <$ string label | (label, dir) <- labels]
        labels = [ ("e", east)
                 , ("w", west)
                 , ("ne", northEast)
                 , ("nw", northWest)
                 , ("se", southEast)
                 , ("sw", southWest)
                 ]

main :: IO ()
main = readFile "input.txt" >>= print . (part1 &&& part2) . prepare
