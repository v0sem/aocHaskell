module Days.Y2025.Day2 where


splitOnChar :: (Char -> Bool) -> String -> [String]
splitOnChar p s = case dropWhile p s of
                        "" -> []
                        s' -> w : splitOnChar p s''
                              where (w, s'') = break p s'

splitMe :: String -> Int -> [String]
splitMe "" _ = []
splitMe me sublen = if sublen > length me then [me]
                        else take sublen me : splitMe (drop sublen me) sublen 

isItDouble :: String -> Bool
isItDouble str = let len = length str in
                        take (len - (len `div` 2) + 1) (drop (len `div` 2) str) == take (len `div` 2) str

repeatingOfCourse :: String -> Int -> [Bool]
repeatingOfCourse _ 0 = []
repeatingOfCourse str sublen = let splitboy = splitMe str sublen in
                                all (==head splitboy) splitboy : repeatingOfCourse str (sublen-1)

getAllBaddies :: Int -> Int -> [Int]
getAllBaddies x y = if x > y then [] else
                        (if isItDouble (show x) then x else 0):getAllBaddies (x+1) y 

getAllBaddiesTwo :: Int -> Int -> [Int]
getAllBaddiesTwo x y = if x > y then [] else
                        (if or (repeatingOfCourse (show x) (length (show x) `div` 2)) then x else 0):getAllBaddiesTwo (x+1) y 

rangeToBaddies :: String -> [Int]
rangeToBaddies range = let fromTo = splitOnChar (=='-') range in
                        getAllBaddies (read $ head fromTo) (read $ last fromTo)


rangeToBaddiesTwo :: String -> [Int]
rangeToBaddiesTwo range = let fromTo = splitOnChar (=='-') range in
                        getAllBaddiesTwo (read $ head fromTo) (read $ last fromTo)

day :: [String] -> Int
day input = sum $ concatMap rangeToBaddies (splitOnChar (==',') $ head input)

dayTwo :: [String] -> Int
dayTwo input = sum $ concatMap rangeToBaddiesTwo (splitOnChar (==',') $ head input)
