module Main where
import Control.Monad (replicateM)
import Data.Maybe (listToMaybe)

entriesSatisfying :: Int -> ([a] -> Bool) -> [a] -> [[a]]
entriesSatisfying n f xs = filter f $ replicateM n xs

part1 :: [Int] -> Maybe Int
part1 = fmap product . listToMaybe . (2 `entriesSatisfying` ((== 2020) . sum))

part2 :: [Int] -> Maybe Int
part2 = fmap product . listToMaybe . (3 `entriesSatisfying` ((== 2020) . sum))

main :: IO ()
main = interact $ show . ((,) <$> part1 <*> part2) . map read . lines
