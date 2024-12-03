module Days.Y2024.Day2 where

import Data.List.Split (splitOn)

parseLine :: String -> [Int]
parseLine line = map (\x -> read x) splitOn " " line

safe :: String -> Bool
safe = True

day :: [String] -> Int
day inputLines = sum (map (\x -> if safe x then 1 else 0) inputLines)
