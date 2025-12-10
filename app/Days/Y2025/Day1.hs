module Days.Y2025.Day1 where

import qualified Data.List as T

mod100 :: Int -> Int
mod100 = (`mod` 100)

div100 :: Int -> Int
div100 x = abs (x `div` 100)

parseBoy :: String -> Int
parseBoy ('L':xs) = -(read xs)
parseBoy ('R':xs) = read xs
parseBoy _ = 0

getState :: Int -> String -> Int
getState x input = x + parseBoy input

stateList :: Int -> [String] -> [Int]
stateList _ [] = []
stateList x (y:ys) = let res = mod100 $ getState x y in
                        (res:stateList res ys)

stateListTwo :: Int -> [String] -> [Int]
stateListTwo _ [] = []
stateListTwo x (y:ys) = let res = mod100 $ getState x y in
                        if x == 0 && "L" `T.isPrefixOf` y then div100 (getState x y) - 1: stateListTwo res ys
                        else if "L" `T.isPrefixOf` y && res == 0 then div100 (getState x y) +1 : stateListTwo res ys
                        else div100 (getState x y) : stateListTwo res ys

day :: [String] -> Int
day input = length $ filter (0 == ) $ stateList 50 input

dayTwo :: [String] -> Int
dayTwo input = sum $ stateListTwo 50 input
