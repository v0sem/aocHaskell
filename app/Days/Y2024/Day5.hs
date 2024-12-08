module Days.Y2024.Day5 where

parseRule :: [Int] -> String -> [Int]
parseRule rules newrule = let (x , y) =  (read $ take 2 newrule, read $ drop 3 newrule)
                              in
                                if x `elem` rules && y `elem` rules then



day :: [String] -> Int
day _ = 0

dayTwo :: [String] -> Int
dayTwo _ = 0
