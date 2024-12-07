module Days.Y2024.Day4 where

import Debug.Trace (trace)

isXMAS :: [String] -> Bool -> (Int, Int) -> (Int, Int) -> Int
isXMAS matrix start (x, y) (dirx, diry)
  | start && matrix!!x!!y /= 'X' = 0
  | x + dirx < 0 || y + diry < 0 || (length matrix - 1) < y + diry || (length $ head matrix) - 1 < x + dirx = 0
  | matrix!!x!!y == 'X' && matrix!!(x+dirx)!!(y+diry) == 'M' = isXMAS matrix False ((x+dirx), (y+diry)) (dirx, diry)
  | matrix!!x!!y == 'M' && matrix!!(x+dirx)!!(y+diry) == 'A' = isXMAS matrix False ((x+dirx), (y+diry)) (dirx, diry)
  | matrix!!x!!y == 'A' && matrix!!(x+dirx)!!(y+diry) == 'S' = 1
  | otherwise = 0

getMatrixLen :: [String] -> (Int, Int)
getMatrixLen matrix = ((length $ head matrix) - 1, (length matrix) -1)

day :: [String] -> Int
day matrix = sum . concatMap processX $ [0..maxx]
  where
    (maxx, maxy) = getMatrixLen matrix

    processX x = concatMap (processY x) [0..maxy]

    processY x y = map (isXMAS matrix True (x, y)) directions

    directions = [(0,1), (0,-1), (1,0), (1,1), (1,-1), (-1,0), (-1,1), (-1,-1)]

isX_MAS :: [String] -> (Int, Int) -> Int
isX_MAS matrix (x,y)
  | matrix!!x!!y /= 'A' = 0
  | x < 1 || x > (length $ head matrix) - 2 || y < 1 || y > length matrix -2 = 0
  | matrix!!(x-1)!!(y+1) == 'M' && matrix!!(x-1)!!(y-1) == 'S' && matrix!!(x+1)!!(y+1) == 'M' && matrix!!(x+1)!!(y-1) == 'S' = 1
  | matrix!!(x-1)!!(y+1) == 'M' && matrix!!(x-1)!!(y-1) == 'M' && matrix!!(x+1)!!(y+1) == 'S' && matrix!!(x+1)!!(y-1) == 'S' = 1
  | matrix!!(x-1)!!(y+1) == 'S' && matrix!!(x-1)!!(y-1) == 'M' && matrix!!(x+1)!!(y+1) == 'S' && matrix!!(x+1)!!(y-1) == 'M' = 1
  | matrix!!(x-1)!!(y+1) == 'S' && matrix!!(x-1)!!(y-1) == 'S' && matrix!!(x+1)!!(y+1) == 'M' && matrix!!(x+1)!!(y-1) == 'M' = 1
  | otherwise = 0

dayTwo :: [String] -> Int
dayTwo matrix = sum . concatMap processX $ [0..maxx]
  where
    (maxx, maxy) = getMatrixLen matrix

    processX x = map (processY x) [0..maxy]

    processY x y = isX_MAS matrix (x, y)
