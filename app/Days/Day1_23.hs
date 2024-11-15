module Days.Day1_23 where

import Data.Char (digitToInt, isDigit)
import Data.List (isPrefixOf)
import Data.Maybe (fromMaybe)

getFirstNum :: [Char] -> Int
getFirstNum [] = 0
getFirstNum (x : xs)
  | isDigit x = digitToInt x
  | otherwise = getFirstNum xs

day :: [String] -> Int
day inputLines = sum (map (\x -> getFirstNum x * 10 + getFirstNum (reverse x)) inputLines)

strToNum :: String -> Maybe Int
strToNum s =
  case filter (`isPrefixOf` s) (map fst numWords) of
    [] -> Nothing
    (x : _) -> Just (fromMaybe 0 (lookup x numWords))
  where
    numWords =
      [ ("one", 1),
        ("two", 2),
        ("three", 3),
        ("four", 4),
        ("five", 5),
        ("six", 6),
        ("seven", 7),
        ("eight", 8),
        ("nine", 9)
      ]

getAllNums :: [Char] -> [Int]
getAllNums [] = []
getAllNums (x : xs)
  | isDigit x = digitToInt x : getAllNums xs
  | Just num <- strToNum (x : xs) = num : getAllNums xs
  | otherwise = getAllNums xs

dayTwo :: [String] -> Int
dayTwo inputLines = sum (map (\x -> head (getAllNums x) * 10 + last (getAllNums x)) inputLines)
