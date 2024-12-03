module Days.Y2024.Day2 where

import Data.List.Split (splitOn)
import Data.List  (sort, nub)

parseLine :: String -> [Int]
parseLine line = map (\x -> read x) (splitOn " " line)

safeDifference :: [Int] -> Bool
safeDifference (x:xs)
  | length xs > 0 = abs (x - head xs) <= 3 && safeDifference xs
  | otherwise = True
safeDifference [] = True

safe :: [Int] -> Bool
safe reports = (sort reports == reports || reverse (sort reports) == reports)
            && length (nub reports) == length reports
            && safeDifference reports

day :: [String] -> Int
day inputLines = sum (map (\x -> if safe (parseLine x) then 1 else 0) inputLines)

pop :: Int -> [a] -> [a]
pop index list = take index list ++ drop (index+1) list

anySafe :: [Int] -> Bool
anySafe report = any (\x -> safe (pop x report)) [0..(len-1)]
                where len = length report

dayTwo :: [String] -> Int
dayTwo inputLines = sum  (map (\x -> if safe (parseLine x) then 1
                            else if anySafe (parseLine x) then 1
                            else 0) inputLines)
