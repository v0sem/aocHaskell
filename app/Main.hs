module Main where

import Days.Y2025.Day4 (day, dayTwo)
import System.IO (hFlush, stdout)
import Network.HTTP.Conduit
import Network.HTTP.Types.Header
import qualified Data.Text.Encoding as TE
import qualified Data.Text as T
import qualified Data.ByteString as BL
import qualified Data.ByteString.Char8 as B

getInput :: String -> String -> IO [String]
getInput daystr year = do
  initReq <- parseRequest ("https://adventofcode.com/" ++ year ++ "/day/" ++ daystr ++ "/input")
  let sessionCookie = "session=53616c7465645f5f0c3e7f507d56ae8b36045311ca2f8e22317bc41908657640b1baf8c88cac4d8df97a30ac46a0735c8821a8a5731788fe697f5be1918a9622"
      req = initReq { requestHeaders = (hCookie, B.pack sessionCookie) : requestHeaders initReq }
  manager <- newManager tlsManagerSettings
  response <- httpLbs req manager
  let text = TE.decodeUtf8 (BL.toStrict $ responseBody response)
  return $ map T.unpack (T.lines text)

main :: IO ()
main = do
  putStrLn "What day do you want to parse?"
  dayInput <- getLine
  putStrLn "What year"
  yearInput <- getLine
  file <- getInput dayInput yearInput
  putStrLn ("Hither shall be thine output: " ++ show (day file))
  putStr "Is the second part completed m'lord? [y/N] "
  hFlush stdout
  yes <- getLine
  if yes == "y" then putStrLn ("And thus here it is: " ++ show (dayTwo file))
    else putStrLn "Then we are done here."
