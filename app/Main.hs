module Main where

import Days.Day1_23 (day, dayTwo)

main :: IO ()
main = do
  putStrLn "What day do you want to parse?"
  dayInput <- getLine
  f <- readFile ("data/day" ++ dayInput ++ ".txt")
  putStrLn ("Hither shall be thine output: " ++ show (day (lines f)))
  putStrLn "Is the second part completed m'lord? [y/n]"
  yes <- getLine
  if yes == "y" then putStrLn ("And thus here it is: " ++ show (dayTwo (lines f)))
    else putStrLn "Then we are done here."
