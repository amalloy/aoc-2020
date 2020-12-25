{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Main where

import Control.Arrow ((&&&))
import Data.Foldable (asum)
import Data.Monoid (Sum(..))
import Data.Maybe (fromMaybe)
import Data.Semigroup (Min(..), Max(..))

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Text.Regex.Applicative

newtype AxialCoord = AxialCoord (Sum Int, Sum Int)
  deriving (Ord, Eq, Monoid, Semigroup, Show)

data Color = Black | White deriving Show

type Input = [AxialCoord]

west, east, northWest, southEast, northEast, southWest :: AxialCoord
west = AxialCoord (-1, 0)
east = AxialCoord (1, 0)
northWest = AxialCoord (0, -1)
southEast = AxialCoord (0, 1)
northEast = northWest <> east
southWest = southEast <> west

dirs :: [AxialCoord]
dirs = [west, east, northWest, northEast, southWest, southEast]

neighbors :: AxialCoord -> [AxialCoord]
neighbors c = (<> c) <$> dirs

newColor :: Color -> Int -> Color
newColor Black 0 = White
newColor Black n | n > 2 = White
newColor White 2 = Black
newColor old _ = old

countBlack :: [Color] -> Int
countBlack = length . filter black
  where black Black = True
        black White = False

type Bound = (Min Int, Min Int, Max Int, Max Int)

type Grid = Map AxialCoord Color

bounds :: [AxialCoord] -> Bound
bounds cs = result
  where Just result = foldMap minMax cs
        minMax (AxialCoord (Sum x, Sum y)) = Just (Min x, Min y, Max x, Max y)

step :: (Bound, Grid) -> (Bound, Grid)
step ((Min minX, Min minY, Max maxX, Max maxY), m) = (bound', m')
  where bound' = (Min $ minX - 1, Min $ minY - 1,
                  Max $ maxX + 1, Max $ maxY + 1)
        get c = M.findWithDefault White c m
        m' = M.fromList $ do
          x <- [minX - 1..maxX + 1]
          y <- [minY - 1..maxY + 1]
          let c = AxialCoord (Sum x, Sum y)
              (this:ns) = map get (c : neighbors c)
              numBlack = countBlack ns
          pure (c, newColor this numBlack)

seed :: Input -> Map AxialCoord Color
seed = M.fromListWith combine . map start
  where start coord = (coord, Black)
        combine Black Black = White
        combine White White = White
        combine _ _ = Black

run :: Int -> Input -> Int
run n = countBlack . M.elems . snd . (!! n) . iterate step . (bounds &&& seed)

part1 :: Input -> Int
part1 = run 0

part2 :: Input -> Int
part2 = run 100

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
