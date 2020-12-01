module Main where

part1 :: [Int] -> [Int]
part1 xs = [x * y | x <- xs, y <- xs, x + y == 2020]

main :: IO ()
main = interact $ show . part1 . map read . lines
