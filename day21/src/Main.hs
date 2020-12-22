{-# LANGUAGE ApplicativeDo #-}

module Main where

import Control.Arrow ((&&&))
import Data.List (foldl', intercalate)
import Data.Maybe (fromJust)

import qualified Data.Set as S
import Data.Set (Set)
import qualified Data.Map.Strict as M
import Data.Map.Strict (Map)

import Text.Regex.Applicative

newtype Ingredient = Ingredient {getIngredient :: String} deriving (Show, Eq, Ord)
newtype Allergen = Allergen {getAllergen :: String} deriving (Show, Eq, Ord)
data Dish = Dish (Set Ingredient) (Set Allergen) deriving Show
data Cuisine = Cuisine [Dish] (Set Ingredient) (Set Allergen) deriving Show

type Input = Cuisine

firstPass :: Cuisine -> Map Allergen (Set Ingredient)
firstPass (Cuisine dishes ingredients allergens)
  = foldl' update (M.fromSet (const ingredients) allergens) dishes
  where update :: Map Allergen (Set Ingredient) -> Dish -> Map Allergen (Set Ingredient)
        update m (Dish is as) = foldl' markMissing m (S.toList as)
          where notPresent = S.difference ingredients is
                markMissing m a = M.adjust (`S.difference` notPresent) a m

part1 :: Input -> Int
part1 cuisine@(Cuisine dishes ingredients _allergens) =
  numSafeIngredients
  where numSafeIngredients = length . filter safe . concatMap contents $ dishes
        contents :: Dish -> [Ingredient]
        contents (Dish is _as) = S.toList is
        safe :: Ingredient -> Bool
        safe = flip S.member safeIngredients
        potentialCauses = firstPass cuisine
        safeIngredients = S.difference ingredients dangerousIngredients
        dangerousIngredients = mconcat (M.elems potentialCauses)

part2 :: Input -> String
part2 cuisine = let links = firstPass cuisine
                    pending = not . all isSingleton . M.elems
                    final = head . dropWhile pending . iterate tighten $ links
                in intercalate "," . map (getIngredient . S.findMin) . M.elems $ final
  where tighten :: Map Allergen (Set Ingredient) -> Map Allergen (Set Ingredient)
        tighten m = let resolved = mconcat . filter isSingleton $ M.elems m
                        update :: Set Ingredient -> Set Ingredient
                        update is | isSingleton is = is
                                  | otherwise = S.difference is resolved
                    in fmap update m
        isSingleton = (== 1) . S.size


mkCuisine :: [Dish] -> Cuisine
mkCuisine dishes = uncurry (Cuisine dishes) $ foldMap recipe dishes
  where recipe (Dish ingredients allergens) = (ingredients, allergens)

prepare :: String -> Input
prepare = mkCuisine . fromJust . traverse (=~ dish) . lines
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
