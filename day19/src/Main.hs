module Main where

import Control.Arrow ((&&&))
import Control.Applicative ((<|>), some)
import qualified Data.IntMap as M
import Data.IntMap.Strict (IntMap)
import Text.Parsec.String (Parser)
import Text.Parsec.Char (digit, char, string, anyChar)
import Text.Parsec.Combinator (sepEndBy, choice)
import Text.Parsec (parse, try, eof)

data Rule = Rule Int Production deriving Show
data Production = Lit Char
                | Alt [[Int]] deriving Show

type ParseTree = ()

int :: Parser Int
int = read <$> some digit

rule :: Parser Rule
rule = do
  label <- int
  _ <- string ": "
  Rule label <$> (lit <|> alt)
  where lit = Lit <$> (char '"' *> anyChar <* char '"')
        alt :: Parser Production
        alt = Alt <$> cat `sepEndBy` (string "| ")
        cat :: Parser [Int]
        cat = int `sepEndBy` (char ' ')

compile :: IntMap Production -> Int -> Parser ParseTree
compile m root = case m M.! root of
  Lit c -> () <$ char c
  Alt options -> () <$ (choice . map (try . cat) $ options)
    where cat = traverse (compile m)

data Input = Input (Parser ParseTree) [String]

part1 :: Input -> Int
part1 (Input p ss) = length [tree | Right tree <- map (parse p "stdin") ss]
--part1 (Input p ss) = map (parse p "stdin") ss

part2 :: Input -> ()
part2 = const ()

prepare :: String -> Input
prepare s = let text = lines s
                (before, ("":after)) = span (/= "") text
                rules = compileRules . zipWith parseRule [0..] $ before
            in Input rules after
  where parseRule :: Int -> String -> Rule
        parseRule lineNum text = case parse rule ("line " ++ show lineNum) text of
          Right x -> x
          Left e -> error (show e)
        compileRules rules = compile m 0 <* eof
          where m = M.fromList [(num, production) | Rule num production <- rules]

main :: IO ()
main = readFile "input.txt" >>= print . (part1 &&& part2) . prepare
