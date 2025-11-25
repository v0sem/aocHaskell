module Days.Y2024.Day5 where

import Data.List.Split

slice :: Int -> Int -> [a] -> [a]
slice from to xs = take (to - from + 1) (drop from xs)

getRules :: [String] -> [String]
getRules input = head (splitOn [""] input)

parseRule :: String -> Bool -> Int
parseRule rule True = read $ slice 0 2 rule
parseRule rule False = read $ slice 3 5 rule

numbersAfterOrBefore :: Int -> Bool -> [String] -> [Int]
numbersAfterOrBefore num after (x:xs)
  | parseRule x after == num = parseRule x (not after) : numbersAfterOrBefore num after xs
  | otherwise = numbersAfterOrBefore num after xs
numbersAfterOrBefore _ _ [] = []

day :: [String] -> Int
day input = (length $ getRules input) - length input

dayTwo :: [String] -> Int
dayTwo _ = 0
