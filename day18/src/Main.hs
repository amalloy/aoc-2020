module Main where

import Control.Arrow ((&&&))
import Control.Applicative ((<|>))
import Data.List (foldl')
import Text.Parsec.String (Parser)
import Text.Parsec.Char
import Text.Parsec (parse, ParseError, many1, many, try)

type Input = [String]

data Expr = Int Int
          | Bin Expr Op Expr deriving Show

data Op = Plus | Times deriving Show

int :: Parser Expr
int = Int . read <$> many1 digit

op :: Parser Op
op = Plus <$ char '+' <|> Times <$ char '*'

chain :: (Expr -> a -> Expr) -> Parser Expr -> Parser a -> Parser Expr
chain combine base repeated = do
  l <- base
  more <- many repeated
  pure $ foldl' combine l more

expr :: Parser Expr
expr = chain build simple ((,) <$> op <*> simple)
  where simple = parens <|> int
        build left (op, right) = Bin left op right
        parens = char '(' *> expr <* char ')'

eval :: Expr -> Int
eval (Int x) = x
eval (Bin x op y) = f (eval x) (eval y)
  where f = case op of Plus -> (+); Times -> (*)

expr2 :: Parser Expr
expr2 = product <|> addend
  where paren = char '(' *> expr2 <* char ')'
        product = try (chain p sum (char '*' *> sum)) <|> sum
          where p l r = Bin l Times r
        sum = try (chain s addend (char '+' *> addend)) <|> addend
          where s l r = Bin l Plus r
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
