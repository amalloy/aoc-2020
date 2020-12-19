{-# LANGUAGE DeriveFunctor #-}

module Main where

import Control.Arrow ((&&&))
import Control.Applicative ((<|>), some, Alternative, empty)
import Control.Monad (ap)
import qualified Data.IntMap as M
import Data.IntMap.Strict (IntMap)
import Data.Foldable (asum)

-- Full-featured, non-backtracking parser, for use in parsing grammar
import Text.Parsec.String (Parser)
import Text.Parsec.Char (digit, char, string, anyChar)
import Text.Parsec.Combinator (sepEndBy)
import Text.Parsec (parse)


-- Fully backtracking parser, for use in parsing the input strings
newtype BParser a = BParser {runParser :: String -> [(a, String)]}
  deriving Functor

instance Applicative BParser where
  pure = return
  (<*>) = ap

instance Monad BParser where
  return x = BParser $ \s -> [(x, s)]
  (BParser p) >>= f = BParser $ \s -> do
    (x, r) <- p s
    runParser (f x) r

instance Alternative BParser where
  empty = BParser $ const []
  p <|> q = BParser $ \s ->
    runParser p s ++ runParser q s

beof :: BParser ()
beof = BParser $ \s -> case s of
  "" -> [((), "")]
  _ -> []

bsatisfy :: (Char -> Bool) -> BParser Char
bsatisfy f = BParser $ \s -> case s of
  (x:r) | f x -> [(x, r)]
  _ -> []

bchar :: Char -> BParser Char
bchar x = bsatisfy (== x)

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

compile :: IntMap Production -> Int -> BParser ParseTree
compile m root = case m M.! root of
  Lit c -> () <$ bchar c
  Alt options -> () <$ (asum . map cat $ options)
    where cat = traverse (compile m)

data Input = Input [Rule] [String]

part1 :: Input -> Int
part1 (Input rules ss) = length [() | tree <- map (runParser p) ss, not $ null tree]
  where p = compile m 0 <* beof
        m = M.fromList [(num, production) | Rule num production <- rules]

part2 :: Input -> Int
part2 (Input rules ss) = length [() | tree <- map (runParser p) ss, not $ null tree]
  where p = (m M.! 0) <* beof
        m :: IntMap (BParser ParseTree)
        m = M.fromList
          (    [(8, knotCompile eight), (11, knotCompile eleven)]
            ++ [ (num, knotCompile production)
               | Rule num production <- rules
               , num `notElem` [8, 11]
               ])
        eight, eleven :: Production
        eight = Alt [[42], [42, 8]]
        eleven = Alt [[42, 11, 31], [42, 31]]
        knotCompile :: Production -> BParser ParseTree
        knotCompile (Lit c) = () <$ bchar c
        knotCompile (Alt options) = () <$ (asum . map (sequence . cat) $ options)
          where cat xs = map (m M.!) xs

prepare :: String -> Input
prepare s = let text = lines s
                (before, ("":after)) = span (/= "") text
                rules = zipWith parseRule [0..] $ before
            in Input rules after
  where parseRule :: Int -> String -> Rule
        parseRule lineNum text = case parse rule ("line " ++ show lineNum) text of
          Right x -> x
          Left e -> error (show e)

main :: IO ()
main = readFile "input.txt" >>= print . (part1 &&& part2) . prepare
