module Main where

import Control.Arrow ((&&&))
import Control.Applicative ((<|>), some)
import Text.Parsec.String (Parser)
import Text.Parsec.Char (digit, char)
import Text.Parsec.Combinator (chainl1)
import Text.Parsec (parse, ParseError)

type Input = [String]

data Expr = Int Int
          | Bin Op Expr Expr deriving Show

data Op = Plus | Times deriving Show

int :: Parser Expr
int = Int . read <$> some digit

eval :: Expr -> Int
eval (Int x) = x
eval (Bin op x y) = f (eval x) (eval y)
  where f = case op of Plus -> (+); Times -> (*)

run :: Parser Expr -> Input -> Either ParseError Int
run p = fmap sum . traverse (fmap eval . parse p "stdin")

part1 :: Input -> Either ParseError Int
part1 = run expr
  where expr = chainl1 (int <|> paren) op
        paren = char '(' *> expr <* char ')'
        op = Bin <$> (Plus <$ char '+' <|> Times <$ char '*')

part2 :: Input -> Either ParseError Int
part2 = run product
  where product = chainl1 sum (Bin Times <$ char '*')
        sum = chainl1 addend (Bin Plus <$ char '+')
        addend = int <|> paren
        paren = char '(' *> product <* char ')'

prepare :: String -> Input
prepare = map (filter (/= ' ')) . lines

main :: IO ()
main = readFile "input.txt" >>= print . (part1 &&& part2) . prepare
