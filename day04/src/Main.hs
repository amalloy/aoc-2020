module Main where

import Control.Arrow ((&&&))
import Data.List.Split (splitOn)
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe)
import Text.Regex.Applicative

type Passport = M.Map String String

type Input = [Passport]

part1 :: Input -> Int
part1 = length . filter valid
  where valid = (== 7) . length . M.keys . M.delete "cid"

part2 :: Input -> ()
part2 i = ()

prepare :: String -> Input
prepare = fromMaybe [] . traverse passport . splitOn "\n\n"
  where passport = fmap M.fromList . (=~ r)
        r :: RE Char [(String, String)]
        r = many field
        field :: RE Char (String, String)
        field = (,) <$>
          (many (psym whitespace)
          *> many (psym (/= ':')) :: RE Char String)
          <*> (sym ':'
          *> many (psym (not . whitespace)) :: RE Char String)
          <* many (psym whitespace)
        whitespace = (`elem` " \n")

main :: IO ()
main = readFile "input.txt" >>= print . (part1 &&& part2) . prepare
