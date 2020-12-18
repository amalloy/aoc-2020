module Main where

import Control.Arrow ((&&&))
import Control.Applicative ((<|>))
import Data.List (foldl')
import Text.Parsec.String (Parser)
import Text.Parsec.Char
import Text.Parsec (parse, ParseError, many1, many)

type Input = [String]

data Expr = Int Int
          | Bin Expr Op Expr deriving Show

data Op = Plus | Times deriving Show

int :: Parser Expr
int = Int . read <$> many1 digit

parens :: Parser Expr
parens = char '(' *> expr <* char ')'

op :: Parser Op
op = Plus <$ char '+' <|> Times <$ char '*'

expr :: Parser Expr
expr = do
  l <- simple
  more <- many ((,) <$> op <*> simple)
  pure $ foldl' build l more
  where simple = parens <|> int
        build left (op, right) = Bin left op right

eval :: Expr -> Int
eval (Int x) = x
eval (Bin x op y) = f (eval x) (eval y)
  where f = case op of Plus -> (+); Times -> (*)

part1 :: Input -> Either ParseError Int
part1 = fmap sum . traverse (fmap eval . parse expr "stdin")

part2 :: Input -> ()
part2 = const ()

prepare :: String -> Input
prepare = map (filter (/= ' ')) . lines

main :: IO ()
main = readFile "input.txt" >>= print . (part1 &&& part2) . prepare
