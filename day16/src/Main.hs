{-# LANGUAGE ApplicativeDo #-}

module Main where

import Control.Arrow ((&&&))
import Data.Char (isDigit)

import qualified Data.IntMap.Strict as M
import qualified Data.Set as  S
import Data.IntMap.Strict (IntMap)
import Data.Set (Set)
import Text.Regex.Applicative

data Field = Field String (Set Int) deriving Show
data Input = Input [Field] Ticket [Ticket] deriving Show
type Ticket = [Int]

index :: [Field] -> IntMap (Set String)
index fields = M.unionsWith (<>) $ do
  Field label values <- fields
  value <- S.elems values
  pure $ M.singleton value (S.singleton label)

part1 :: Input -> Int
part1 (Input fields _ tickets) = sum . filter (`M.notMember` ix) $ concat tickets
  where ix = index fields

part2 :: Input -> ()
part2 = const ()

prepare :: String -> Maybe Input
prepare = (=~ input)
  where input = do
          fields <- many field
          string "\nyour ticket:\n"
          yours <- ticket
          string "\nnearby tickets:\n"
          others <- many ticket
          pure $ Input fields yours others
        ticket = int `sepBy` (sym ',') <* sym '\n'
        int = read <$> many (psym isDigit)
        sepBy p sep = (:) <$> p <*> many (sep *> p)
        field = do
          label <- many (psym (/= ':'))
          string ": "
          lo <- intRange
          string " or "
          hi <- intRange
          sym '\n'
          pure $ Field label $ S.fromDistinctAscList (lo ++ hi)
        intRange = do
          lo <- int
          sym '-'
          hi <- int
          pure [lo..hi]

main :: IO ()
main = readFile "input.txt" >>= print . fmap (part1 &&& part2) . prepare
