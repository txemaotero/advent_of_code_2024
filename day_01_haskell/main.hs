import Data.List (sort)

splitOnce :: String -> (Int, Int)
splitOnce line = let (a, b) = break (== ' ') line in (read a, read b)

splitTwoLists :: [String] -> ([Int], [Int])
splitTwoLists lines = unzip (map splitOnce lines)

sumAbsDiff :: [Int] -> [Int] -> Int
sumAbsDiff l1 l2 = sum (zipWith (\x y -> abs (x - y)) l1 l2)

count :: (Eq a) => a -> [a] -> Int
count x = length . filter (== x)

similarityScore :: [Int] -> Int -> Int
similarityScore list x = x * (count x list)

sumScores :: [Int] -> [Int] -> Int
sumScores l1 l2 = sum (map (similarityScore l2) l1)

main :: IO ()
main = do
  content <- readFile "input.txt"
  let linesOfFile = lines content
  let (l1, l2) = splitTwoLists linesOfFile
  putStr "Part 1: "
  print (sumAbsDiff (sort l1) (sort l2))
  putStr "\nPart 2: "
  print (sumScores l1 l2)
