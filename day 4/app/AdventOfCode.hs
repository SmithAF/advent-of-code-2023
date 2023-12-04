module AdventOfCode (computeResult) where
import Data.List.Split (splitOn)

computeResult :: String -> Int
computeResult str = do
  let games = removeGameLabels (splitGames str)




  0



--code for removing the Game <int>: from the strings

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

mapGameDataToTupleString :: String -> (String, String)
mapGameDataToTupleString str =  (\(winningNumbers: numbers : xs) -> (winningNumbers, numbers)) (splitOnGameDivider str)


--code for mapping the numbers into Ints

toInt :: String -> Int
toInt = read

splitOnSpace :: String -> [String]
splitOnSpace = splitOn " "




