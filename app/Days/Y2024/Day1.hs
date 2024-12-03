module Days.Y2024.Day1 where

import Data.List (sort)

slice :: Int -> Int -> [a] -> [a]
slice from to xs = take (to - from) (drop from xs)

getListL :: [String] -> [Int]
getListL input = sort (map (\x -> read (slice 0 5 x)) input)

getListR :: [String] -> [Int]
getListR input = sort (map (\x -> read (slice 8 13 x)) input)


day :: [String] -> Int
day inputLines = (sum (map
                       (\(x, y) -> abs (x - y))
                       (zip (getListL inputLines)
                            (getListR inputLines))))

numOcc :: Eq a => a -> [a] -> Int
numOcc x = length . filter (== x)

dayTwo :: [String] -> Int
dayTwo inputLines = sum (map
                         (\x -> x * numOcc x (getListR inputLines))
                         (getListL inputLines))
