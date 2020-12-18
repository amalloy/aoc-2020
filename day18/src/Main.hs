module Main where

import Control.Arrow ((&&&))
import Control.Applicative ((<|>))
import Text.Parsec.String (Parser)
import Text.Parsec.Char
import Text.Parsec.Combinator (chainl1)
import Text.Parsec (parse, ParseError, many1)

type Input = [String]

data Expr = Int Int
          | Bin Expr Op Expr deriving Show

data Op = Plus | Times deriving Show

int :: Parser Expr
int = Int . read <$> many1 digit

op :: Parser (Expr -> Expr -> Expr)
op = flip Bin <$> (Plus <$ char '+' <|> Times <$ char '*')

expr :: Parser Expr
expr = chainl1 (parens <|> int) op
  where parens = char '(' *> expr <* char ')'

eval :: Expr -> Int
eval (Int x) = x
eval (Bin x op y) = f (eval x) (eval y)
  where f = case op of Plus -> (+); Times -> (*)

expr2 :: Parser Expr
expr2 = product <|> addend
  where paren = char '(' *> expr2 <* char ')'
        product = chainl1 sum (p <$ char '*')
          where p = flip Bin Times
        sum = chainl1 addend (s <$ char '+')
          where s = flip Bin Plus
        addend = int <|> paren

run :: Parser Expr -> Input -> Either ParseError Int
run p = fmap sum . traverse (fmap eval . parse p "stdin")

part1 :: Input -> Either ParseError Int
part1 = run expr

part2 :: Input -> Either ParseError Int
part2 = run expr2

prepare :: String -> Input
prepare = map (filter (/= ' ')) . lines

main :: IO ()
main = readFile "input.txt" >>= print . (part1 &&& part2) . prepare
