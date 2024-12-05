module Days.Y2024.Day3 where

import Data.Char (isDigit)
import Data.List (isPrefixOf, findIndex)
import Debug.Trace (trace)

isAllDigit :: String -> Bool
isAllDigit = all isDigit

parseMul :: String -> [Int]
parseMul (x:xs)
  | x == 'm' = parseMul (drop 3 xs)
  | isDigit x =
    case findIndex (== ',') (x:xs) of
      Just idx ->
        let upToComma = take (idx) (x:xs)
            fromComma = drop (idx + 1) (x:xs)
        in if isAllDigit upToComma && isAllDigit fromComma then
                [read upToComma, read fromComma]
           else [0]
      Nothing -> [0]
  | otherwise = [0]
parseMul [] = [0]

getMults :: String -> Bool -> Bool -> [[Int]]
getMults str@(_:xs) enabler enable
  | "mul(" `isPrefixOf` str && (enabler || enable) =
    case findIndex (== ')') str of
      Just idx ->
        let upToIdx = take idx str
            parsed = parseMul upToIdx
        in parsed : getMults xs enabler enable
      Nothing -> getMults xs enabler enable
  | "do()" `isPrefixOf` str = getMults xs enabler True
  | "don't()" `isPrefixOf` str = getMults xs enabler False
  | otherwise = getMults xs enabler enable
getMults [] _ _ = []

day :: [String] -> Int
day input = sum (map (\x -> foldr (*) 1 x) mults)
  where mults = filter (/= [0]) (concat (map (\x -> getMults x True False) input))

dayTwo :: [String] -> Int
dayTwo input = sum (map (\x -> foldr (*) 1 x) mults)
  where mults = filter (/= [0]) (getMults (foldr (++) "" input) False True)
