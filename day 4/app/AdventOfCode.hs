module AdventOfCode (computeResult, computeResultP2) where
import Data.List.Split (splitOn)
import Data.Void (Void)

computeResult :: String -> Int
computeResult str =  sumScores $ mapWinningNumbers $ mapGamesToTuples $ removeGameLabels (splitGames str)


computeResultP2 :: [Char] -> Int
computeResultP2 str = do
  let games = mapIndexToEach $ mapWinningNumbers $ mapGamesToTuples $ removeGameLabels (splitGames str)  
  length (mapRemoveIndex (cloneMachine games games games))
  


cloneMachine :: [(Int, [Int])] -> [(Int, [Int])] -> [(Int, [Int])] -> [(Int, [Int])]
cloneMachine list list2 output
  | remaining == 0 = output
  | otherwise = cloneMachine list computedList output ++ computedList
  where
    computedList = cloneList list list2
    remaining = length computedList

takeWinnersAtPosition :: [Int] -> Int -> [[Int]] -> [[Int]]
takeWinnersAtPosition x i list = take (length x) (drop i list)

addIndexToEach :: [[Int]] -> Int -> [(Int, [Int])] -> [(Int, [Int])]
addIndexToEach (x : xs) i output
  | null xs =  output ++ [(i, x)]
  | otherwise = addIndexToEach xs (i+1) output ++ [(i, x)]

removeIndex :: (Int, [Int]) -> [Int]
removeIndex (_, game) = game

mapRemoveIndex :: [(Int, [Int])] -> [[Int]]
mapRemoveIndex = map removeIndex

mapIndexToEach :: [[Int]] -> [(Int, [Int])]
mapIndexToEach games = reverse (addIndexToEach games 1 [])

cloneGames :: [(Int, [Int])] -> (Int, [Int]) -> [(Int, [Int])]
cloneGames list (index, games) = take (length games) (drop index list)

cloneList :: [(Int, [Int])] -> [(Int, [Int])] -> [(Int, [Int])]
cloneList list = concatMap (cloneGames list)

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

calculateScore :: [Int] -> Int
calculateScore x
  | winners == 0 = 0
  | otherwise = 2 ^ (winners-1)
  where winners = length x

mapScores :: [[Int]] -> [Int]
mapScores = map calculateScore

sumScores :: [[Int]] -> Int
sumScores x = sum (mapScores x)



