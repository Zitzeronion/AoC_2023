{-# LANGUAGE OverloadedStrings #-}
import qualified Data.Text as T
import Data.List

main :: IO ()
main = do
  contents <- readFile "day1_input.txt"
  let linesOfFiles = lines contents
  -- print linesOfFiles
  let tLines = [T.pack c | c <- linesOfFiles]
  -- print tLines
  -- print numOc
  let complexOut = [ T.replace (T.pack "one") (T.pack "o1e") c | c <- tLines ]
  let complexOut2 = [ T.replace (T.pack "two") (T.pack "t2o") c | c <- complexOut ]
  let complexOut3 = [ T.replace (T.pack "three") (T.pack "t3e") c | c <- complexOut2 ]
  let complexOut4 = [ T.replace (T.pack "four") (T.pack "f4r") c | c <- complexOut3 ]
  let complexOut5 = [ T.replace (T.pack "five") (T.pack "f5e") c | c <- complexOut4 ]
  let complexOut6 = [ T.replace (T.pack "six") (T.pack "s6x") c | c <- complexOut5 ]
  let complexOut7 = [ T.replace (T.pack "seven") (T.pack "s7n") c | c <- complexOut6 ]
  let complexOut8 = [ T.replace (T.pack "eight") (T.pack "e8t") c | c <- complexOut7 ]
  let complexOut9 = [ T.replace (T.pack "nine") (T.pack "n9e") c | c <- complexOut8 ]
  print complexOut9
  let linewc = getMeNumbersText complexOut9
  print linewc
  
  let nOnes = getFirstInts linewc
  let nLasts = getLastInts linewc
  let realFirsts = example nOnes
  let realLasts = example nLasts
  -- print realFirsts
  -- print realLasts
  let both = zipWith (++) realFirsts realLasts
  -- let newLines = getTwoInts linewc
  -- print both
  let sumN = [stringToInt s | s <- both]
  let result = sum sumN
  -- print sumN
  putStrLn "The final result is:"
  print result
  
removeChars st = [ c | c <- st, c `elem` ['1'..'9']]
getMeNumbers st = [ removeChars c | c <- st]
getMeNumbersText st = [ removeChars (T.unpack c) | c <- st]
getFirstInts st = [head c | c <- st] 
getLastInts st = [last c | c <- st] 

example :: String -> [String]
example s = map (\c -> [c]) s

stringToInt :: String -> Int
stringToInt s = read s 