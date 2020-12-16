{-# LANGUAGE ApplicativeDo #-}

module Main where

import Control.Arrow ((&&&))
import Control.Monad (guard)
import Data.Char (isDigit)
import Data.List (isPrefixOf)
import Data.Maybe (listToMaybe)

import qualified Data.IntMap.Strict as M
import qualified Data.Set as  S
import Data.IntMap.Strict (IntMap)
import Data.Set (Set)
import Text.Regex.Applicative

data Field = Field String (Set Int) deriving Show
data Input = Input [Field] Ticket [Ticket] deriving Show
type Ticket = [Int]
type Index = IntMap (Set String)
newtype Constraints = Constraints {fromConstraint :: IntMap (Set String)} deriving Show

index :: [Field] -> IntMap (Set String)
index fields = M.unionsWith (<>) $ do
  Field label values <- fields
  value <- S.elems values
  pure $ M.singleton value (S.singleton label)

part1 :: Input -> Int
part1 (Input fields _ tickets) = sum . filter (`M.notMember` ix) $ concat tickets
  where ix = index fields

ticketToConstraint :: Index -> Ticket -> Constraints
ticketToConstraint ix t = Constraints . M.fromDistinctAscList $ do
  (i, v) <- zip [0..] t
  pure (i, M.findWithDefault S.empty v ix)

isSingleton :: Set a -> Bool
isSingleton = (== 1) . S.size

resolve :: S.Set String -> [Constraints] -> [(Int, String)]
resolve done cs = do
  let fixed = M.unionsWith S.intersection [m | Constraints m <- cs]
  (i, options) <- M.assocs fixed
  guard $ isSingleton options
  let field = S.findMin options
  guard $ not (S.member field done)
  pure (i, field)

resolveConstraints :: Index -> [Ticket] -> [String]
resolveConstraints ix ts = go S.empty $ map (ticketToConstraint ix) ts
  where go resolvedFields cs@(c:_) | all isSingleton . fromConstraint $  c
          = concatMap S.toList . M.elems $ fromConstraint c
                                   | otherwise
          = case listToMaybe (resolve resolvedFields cs) of
              Nothing ->
                error $ "gave up after resolving only " ++ show resolvedFields
              Just (i, label) ->
                go (S.insert label resolvedFields) (map (solved i label) cs)
        solved :: Int -> String -> Constraints -> Constraints
        solved i label (Constraints m) = Constraints
          . M.insert i (S.singleton label)
          . fmap (S.delete label) $ m

part2 :: Input -> Int
part2 (Input fields yours others) =
  let ix = index fields
      valid = filter (all (`M.member` ix)) others
      fieldOrder = resolveConstraints ix valid
      mine = zip fieldOrder yours
  in product . map snd . filter (("departure" `isPrefixOf`) . fst) $ mine

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
