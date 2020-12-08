{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE LambdaCase #-}

module Main where

import Control.Arrow ((&&&))
import Data.Char (isDigit)
import Data.Maybe (fromMaybe)

import qualified Data.Set as Set
import qualified Data.Sequence as Seq

import Text.Regex.Applicative

data Code = Acc | Jmp | Nop deriving Show
data Instruction = Instr Code Int deriving Show
data State = State {acc, pc :: Int, visited :: Set.Set Int} deriving Show

type Program = Seq.Seq Instruction
type Input = Program

initialState :: State
initialState = State 0 0 Set.empty

step :: Program -> State -> State
step prog (State a p v) =
  case code of
    Nop -> State a (succ p) visit'
    Acc -> State (a + arg) (succ p) visit'
    Jmp -> State a (p + arg) visit'
  where Instr code arg = Seq.index prog p
        visit' = Set.insert p v

complete :: Program -> State -> State
complete prog = head . dropWhile (not . done) . iterate (step prog)
  where done (State _ p v) = Set.member p v

part1 :: Input -> Int
part1 = acc . (`complete` initialState)

part2 :: Input -> ()
part2 = const ()

prepare :: String -> Input
prepare = Seq.fromList . fromMaybe [] . traverse parse . lines
  where parse = (=~ instruction)
        instruction = Instr <$> code <*> (sym ' ' *> int)
        code = (Acc <$ string "acc") <|> (Jmp <$ string "jmp") <|> (Nop <$ string "nop")
        int = sign <*> (read <$> many (psym isDigit))
        sign = (id <$ sym '+') <|> (negate <$ sym '-')

main :: IO ()
main = readFile "input.txt" >>= print . (part1 &&& part2) . prepare
