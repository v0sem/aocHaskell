module Days.Y2025.Day3 where
import Data.Char (digitToInt)

deleteNth :: Int -> [Int] -> [Int]
deleteNth n xs = let (a, b) = splitAt n xs in a ++ tail b

getBigBoy :: [Int] -> Int
getBigBoy [x] = x
getBigBoy (x:x':xs) = getBigBoy (max x x': xs)
getBigBoy [] = 0

getSmallBoy :: [Int] -> Int
getSmallBoy [x] = x
getSmallBoy (x:x':xs) = getSmallBoy (min x x': xs)
getSmallBoy [] = 0

getFirstPos :: [Int] -> Int -> Int
getFirstPos [] _ = error "Not there"
getFirstPos (x:xs) num
  | x /= num = 1 + getFirstPos xs num
  | otherwise = 0

getNum :: [Int] -> Int
getNum l = let num  = getBigBoy (take (length l-1) l) in
             (num * 10 )+ getBigBoy (drop (getFirstPos l num + 1) l)

removePetites :: [Int] -> Int -> [Int]
removePetites l maxnums
  | maxnums == length l = l
  | maxnums > length l = error "Removed too much"
  | otherwise = removePetites (deleteNth (getFirstPos l (getSmallBoy l)) l) maxnums

getNums :: [Int] -> Int -> [Int]
getNums _ 0 = []
getNums l numnums = let num = getBigBoy (take (length l-(numnums-1)) l) in
                      num : getNums (drop ((getFirstPos l num) + 1) l) (numnums-1)

numsToNum :: [Int] -> Int
numsToNum [] = 0
numsToNum (x:xs) = x + 10 * numsToNum xs

strToNumList :: String -> [Int]
strToNumList = map digitToInt

day :: [String] -> Int
day input = sum $ map (getNum . strToNumList) input

dayTwo :: [String] -> Int
dayTwo input = sum $ map (\x -> numsToNum $ reverse $ getNums (strToNumList x) 12) input
