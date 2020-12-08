{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE LambdaCase #-}

module Main where

import Control.Arrow ((&&&))
import Data.Char (isDigit)
import Data.Foldable (toList)
import Data.Maybe (fromMaybe)

import qualified Data.Set as Set
import qualified Data.Sequence as Seq

import Text.Regex.Applicative

data Code = Acc | Jmp | Nop deriving Show
data Instruction = Instr Code Int deriving Show
data State = State {acc, pc :: Int, visited :: Set.Set Int} deriving Show
data StopReason = InfiniteLoop | Finished | Segfault
data Mode = Running | StopReason StopReason

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

complete :: Program -> State -> (Mode, State)
complete prog =
  head . dropWhile (running . fst) . map addLabel . iterate (step prog)
  where running Running = True
        running _ = False
        addLabel :: State -> (Mode, State)
        addLabel s = (label s, s)
        label (State _ p v) | Set.member p v = StopReason InfiniteLoop
                            | p == size = StopReason Finished
                            | p < 0 || p > size = StopReason Segfault
                            | otherwise = Running
        size = Seq.length prog

allFixes :: Program -> [Program]
allFixes = map Seq.fromList . fix . toList
  where
    fix [] = []
    fix (i@(Instr code arg):prog) =
      case code of
        Nop -> (Instr Jmp arg : prog) : go
        Jmp -> (Instr Nop arg : prog) : go
        Acc -> go
      where go = (i:) <$> fix prog

part1 :: Input -> Int
part1 = acc . snd . (`complete` initialState)

part2 :: Input -> [Int]
part2 =
  map (acc . snd)
  . filter (succeed . fst)
  . map (`complete` initialState)
  . allFixes
  where succeed :: Mode -> Bool
        succeed (StopReason Finished) = True
        succeed _ = False

prepare :: String -> Input
prepare = Seq.fromList . fromMaybe [] . traverse parse . lines
  where parse = (=~ instruction)
        instruction = Instr <$> code <*> (sym ' ' *> int)
        code = (Acc <$ string "acc") <|> (Jmp <$ string "jmp") <|> (Nop <$ string "nop")
        int = sign <*> (read <$> many (psym isDigit))
        sign = (id <$ sym '+') <|> (negate <$ sym '-')

main :: IO ()
main = readFile "input.txt" >>= print . (part1 &&& part2) . prepare
