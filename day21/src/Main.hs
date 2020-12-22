{-# LANGUAGE ApplicativeDo #-}

module Main where

import Control.Arrow ((&&&))
import Data.List (foldl')
import Data.Maybe (fromJust)

import qualified Data.Set as S
import Data.Set (Set)
import qualified Data.Map.Strict as M
import Data.Map.Strict (Map)

import Text.Regex.Applicative

newtype Ingredient = Ingredient String deriving (Show, Eq, Ord)
newtype Allergen = Allergen String deriving (Show, Eq, Ord)
data Dish = Dish (Set Ingredient) (Set Allergen) deriving Show
data Cuisine = Cuisine [Dish] (Set Ingredient) (Set Allergen) deriving Show

type Input = Cuisine

part1 :: Input -> Int
part1 (Cuisine dishes ingredients allergens) =
  numSafeIngredients
  where numSafeIngredients = length . filter safe . concatMap contents $ dishes
        contents :: Dish -> [Ingredient]
        contents (Dish is _as) = S.toList is
        safe :: Ingredient -> Bool
        safe = flip S.member safeIngredients
        safeIngredients = S.difference ingredients dangerousIngredients
        dangerousIngredients = mconcat (M.elems potentialCauses)
        potentialCauses :: Map Allergen (Set Ingredient)
        potentialCauses = foldl' update (M.fromSet (const ingredients) allergens) dishes
        update :: Map Allergen (Set Ingredient) -> Dish -> Map Allergen (Set Ingredient)
        update m (Dish is as) = foldl' markMissing m (S.toList as)
          where notPresent = S.difference ingredients is
                markMissing m a = M.adjust (`S.difference` notPresent) a m

part2 :: Input -> ()
part2 = const ()

cuisine :: [Dish] -> Cuisine
cuisine dishes = uncurry (Cuisine dishes) $ foldMap recipe dishes
  where recipe (Dish ingredients allergens) = (ingredients, allergens)

prepare :: String -> Input
prepare = cuisine . fromJust . traverse (=~ dish) . lines
  where dish = Dish <$> ingredients <* sym ' ' <*> allergens
        ingredients = S.fromList <$> ingredient `sepBy` (sym ' ')
        allergens = do
          string "(contains "
          result <- S.fromList <$> allergen `sepBy` (string ", ")
          sym ')'
          pure result
        ingredient = Ingredient <$> token
        allergen = Allergen <$> token
        sepBy datum sep = do
          x <- datum
          rest <- many (sep *> datum)
          pure $ x : rest
        token = some (psym (/= ' '))

main :: IO ()
main = readFile "input.txt" >>= print . (part1 &&& part2) . prepare
