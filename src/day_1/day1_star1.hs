main :: IO ()
main = do
  contents <- readFile "day1_input.txt"
  
  let linesOfFiles = lines contents
  -- let nlines = (length linesOfFiles)
  let linewc = getMeNumbers linesOfFiles
  print linewc
  let nOnes = getFirstInts linewc
  let nLasts = getLastInts linewc
  let realFirsts = example nOnes
  let realLasts = example nLasts
  print realFirsts
  print realLasts
  let both = zipWith (++) realFirsts realLasts
  -- let newLines = getTwoInts linewc
  print both
  let sumN = [stringToInt s | s <- both]
  let result = sum sumN
  print sumN
  putStrLn "The final result is:"
  print result
  
removeChars st = [ c | c <- st, c `elem` ['1'..'9']]
getMeNumbers st = [ removeChars c | c <- st]
getFirstInts st = [head c | c <- st] 
getLastInts st = [last c | c <- st] 
getBoth st ts = [s ++ t | s <- st, t <- ts]

example :: String -> [String]
example s = map (\c -> [c]) s

stringToInt :: String -> Int
stringToInt s = read s 