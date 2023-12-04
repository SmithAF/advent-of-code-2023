module AdventOfCode (computeResult) where
import Data.List.Split (splitOn)

computeResult :: String -> Int
computeResult str =  sumScores $ mapWinningNumbers $ mapGamesToTuples  $ removeGameLabels (splitGames str)



--code for removing the Game label: from the strings

splitGames :: String -> [String]
splitGames = lines

splitOnGameLabel :: String -> [String]
splitOnGameLabel = splitOn ":"

removeGameLabel :: [String] -> String
removeGameLabel (_ : game : xs) = game

removeGameLabels :: [String] -> [String]
removeGameLabels = map (removeGameLabel . splitOnGameLabel)

--code for dividing the winning numbers from the scratch card numbers

splitOnGameDivider :: String -> [String]
splitOnGameDivider = splitOn "|"

splitOnSpace :: String -> [String]
splitOnSpace = filterOutEmptyStrings . splitOn " "

filterOutEmptyStrings = filter (/= "")

toInt :: String -> Int
toInt = read

listOfNumberStringsToIntList :: [String] -> [Int]
listOfNumberStringsToIntList = map toInt

mapGameDataToTuples :: String -> ([Int], [Int])
mapGameDataToTuples str =  (\(winningNumbers: numbers : xs) -> (listOfNumberStringsToIntList $ splitOnSpace winningNumbers, listOfNumberStringsToIntList $ splitOnSpace numbers)) (splitOnGameDivider str)

mapGamesToTuples :: [String] -> [([Int], [Int])]
mapGamesToTuples = map mapGameDataToTuples

getWinningNumbers :: ([Int], [Int]) -> [Int]
getWinningNumbers (winners, chances) = filter (`elem` winners) chances

mapWinningNumbers :: [([Int], [Int])] -> [[Int]]
mapWinningNumbers = map getWinningNumbers

doubleNumber :: Int -> Int -> Int
doubleNumber i n 
  | n == 0 = i
  | otherwise = doubleNumber (i*2) (n-1)

calculateScore :: [Int] -> Int
calculateScore x
  | winners == 0 = 0  
  | otherwise = doubleNumber 1 (winners - 1)
  where winners = length x

mapScores :: [[Int]] -> [Int]
mapScores = map calculateScore

sumScores :: [[Int]] -> Int
sumScores x = sum (mapScores x)



