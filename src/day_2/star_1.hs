{-# LANGUAGE OverloadedStrings #-}
import qualified Data.Text as T
import Data.List

main :: IO ()
main = do
  contents <- readFile "example.txt"
  let linesOfFiles = lines contents
  print linesOfFiles
  let tLines = [T.pack c | c <- linesOfFiles]
  let complexOut = [ T.replace (T.pack "blue") (T.pack ".") c | c <- tLines ]
  let complexOut2 = [ T.replace (T.pack "green") (T.pack "+") c | c <- complexOut ]
  let complexOut3 = [ T.replace (T.pack "red") (T.pack "-") c | c <- complexOut2 ]
  let simpler = getMeNumbers ([T.unpack c| c <- complexOut3])
  print simpler
  let splitText = [T.words c | c <- tLines]
  print splitText
  let result = 1
  -- print sumN
  putStrLn "The final result is:"
  print result
  
removeChars st = [ c | c <- st, c `elem` ['0','1','2','3','4','5','6','7','8','9',':', '.', '+', '-']]
getMeNumbers st = [ removeChars c | c <- st]
getMeNumbersText st = [ removeChars (T.unpack c) | c <- st]
getFirstInts st = [head c | c <- st] 
getLastInts st = [last c | c <- st] 

example :: String -> [String]
example s = map (\c -> [c]) s

stringToInt :: String -> Int
stringToInt s = read s 

gameRed x
    | x > 12 = False
    | otherwise = True

gameGreen x
    | x > 13 = False
    | otherwise = True

gameBlue x
    | x > 14 = False
    | otherwise = True

