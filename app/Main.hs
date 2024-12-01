module Main where

import Days.Y2024.Day1 (day, dayTwo)
import System.IO (hFlush, stdout)

main :: IO ()
main = do
  putStrLn "What day do you want to parse?"
  dayInput <- getLine
  f <- readFile ("data/day" ++ dayInput ++ ".txt")
  putStrLn ("Hither shall be thine output: " ++ show (day (lines f)))
  putStr "Is the second part completed m'lord? [y/N] "
  hFlush stdout
  yes <- getLine
  if yes == "y" then putStrLn ("And thus here it is: " ++ show (dayTwo (lines f)))
    else putStrLn "Then we are done here."
