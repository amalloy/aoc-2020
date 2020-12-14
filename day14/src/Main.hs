{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE LambdaCase #-}

module Main where

import Control.Arrow ((&&&))
import Control.Monad (replicateM)
import Data.Bits (setBit, clearBit, bit, testBit)
import Data.Bool (bool)
import Data.Char (isDigit)
import Data.List (foldl')
import Data.Maybe (fromMaybe)

import Text.Regex.Applicative
import qualified Data.IntMap as M
import Data.IntMap (IntMap)


data Instruction = Write {address, value :: Int} | SetMask Mask deriving Show
data Trit = X | One | Zero deriving Show
newtype Mask = Mask [Trit] deriving Show

data Computer = Computer Mask (IntMap Int)

type Input = [Instruction]

maskSize :: Int
maskSize = 36

powers :: [Int]
powers = [maskSize - 1, maskSize - 2..0]

interpret :: Trit -> Int -> (Int -> Int)
interpret X = const id
interpret One = flip setBit
interpret Zero = flip clearBit

apply :: Mask -> (Int -> Int)
apply (Mask trits) = foldr (.) id $ zipWith interpret trits powers

step :: Computer -> Instruction -> Computer
step (Computer mask mem) = \case
  SetMask mask' -> Computer mask' mem
  Write addr val -> Computer mask (M.insert addr (apply mask val) mem)

applyFloating :: Mask -> Int -> [Int]
applyFloating (Mask trits) n = map sum $ sequence bits
  where bits = zipWith interpretFloating trits powers
        interpretFloating t p = case t of
          Zero -> [bool 0 on $ testBit n p]
          One -> [on]
          X -> [0, on]
          where on = bit p

stepFloating :: Computer -> Instruction -> Computer
stepFloating (Computer mask mem) = \case
  SetMask mask' -> Computer mask' mem
  Write addr val -> Computer mask (M.union writes mem)
    where writes = M.fromList [(a, val) | a <- applyFloating mask addr]

run :: (Computer -> Instruction -> Computer) -> Input -> Int
run f = sumMem . foldl' f (Computer (Mask $ replicate maskSize X) M.empty)
  where sumMem (Computer _ mem) = sum mem

part1 :: Input -> Int
part1 = run step

part2 :: Input -> Int
part2 = run stepFloating

prepare :: String -> Input
prepare = fromMaybe [] . traverse parse . lines
  where parse = (=~ instruction)
        instruction = setMask <|> writeMem
        setMask = do
          string "mask = "
          trits <- replicateM maskSize trit
          pure (SetMask (Mask trits))
        trit = (One <$ sym '1') <|> (Zero <$ sym '0') <|> (X <$ sym 'X')
        writeMem = do
          string "mem["
          addr <- int
          string "] = "
          value <- int
          pure $ Write addr value
        int = read <$> many (psym isDigit)

main :: IO ()
main = readFile "input.txt" >>= print . (part1 &&& part2) . prepare
