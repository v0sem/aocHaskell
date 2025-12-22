module Days.Y2025.Day4 where

charAtxy :: [[Char]] -> Int -> Int -> Char
charAtxy _ (-1) _ = '.'
charAtxy _ _ (-1) = '.'
charAtxy grid x y
 | x >= length  (head grid) = '.'
 | y >= length grid = '.'
 | otherwise = grid!!y!!x

cartProd :: [Int] -> [Int] -> [(Int, Int)]
cartProd xs ys = [(x,y) | x<-xs, y <-ys, (x,y) /= (0,0)]

getRounders :: [[Char]] -> Int -> Int -> [Char]
getRounders grid x y = map (\(i,j) -> charAtxy grid (x + i) (y + j)) (cartProd [-1,0,1] [-1,0,1])

isAccesible :: [[Char]] -> Int -> Int -> Bool
isAccesible grid x y
  | charAtxy grid x y == '.' = False
  | otherwise = length  (filter (=='@') (getRounders grid x y)) < 4

removeRoll :: [[Char]] -> Int -> Int -> [[Char]]
removeRoll grid x y = splitAt y grid

day :: [String] -> Int
day input =length $ filter (==True) [isAccesible input x y | x <- [0..(length (head input))], y <- [0..length input]]

dayTwo :: [String] -> Int
dayTwo _ = 0
