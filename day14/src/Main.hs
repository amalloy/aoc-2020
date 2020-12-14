{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE LambdaCase #-}

module Main where

import Control.Arrow ((&&&))
import Control.Monad (replicateM)
import Data.Bits (setBit, clearBit)
import Data.Char (isDigit)
import Data.List (foldl')
import Data.Maybe (fromMaybe)

import Text.Regex.Applicative
import qualified Data.IntMap as M
import Data.IntMap (IntMap)


data Instruction = Write {address, value :: Int} | SetMask Mask deriving Show
data Trit = Pass | Set | Clear deriving Show
newtype Mask = Mask [Trit] deriving Show

data Computer = Computer Mask (IntMap Int)

type Input = [Instruction]

maskSize :: Int
maskSize = 36

interpret :: Trit -> Int -> (Int -> Int)
interpret Pass = const id
interpret Set = flip setBit
interpret Clear = flip clearBit

apply :: Mask -> (Int -> Int)
apply (Mask trits) = let fs = zipWith interpret trits [maskSize - 1, maskSize - 2..0]
                     in flip (foldl' (flip ($))) fs

step :: Computer -> Instruction -> Computer
step (Computer mask mem) = \case
  SetMask mask' -> Computer mask' mem
  Write addr val -> Computer mask (M.insert addr (apply mask val) mem)

part1 :: Input -> Int
part1 = sumMem . foldl' step (Computer (Mask $ replicate maskSize Pass) M.empty)
  where sumMem (Computer _ mem) = sum mem

part2 :: Input -> ()
part2 = const ()

prepare :: String -> Input
prepare = fromMaybe [] . traverse parse . lines
  where parse = (=~ instruction)
        instruction = setMask <|> writeMem
        setMask = do
          string "mask = "
          trits <- replicateM maskSize trit
          pure (SetMask (Mask trits))
        trit = (Set <$ sym '1') <|> (Clear <$ sym '0') <|> (Pass <$ sym 'X')
        writeMem = do
          string "mem["
          addr <- int
          string "] = "
          value <- int
          pure $ Write addr value
        int = read <$> many (psym isDigit)

main :: IO ()
main = readFile "input.txt" >>= print . (part1 &&& part2) . prepare
