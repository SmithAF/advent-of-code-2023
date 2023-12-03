{-# LANGUAGE OverloadedStrings #-}
import System.IO
import Data.Text (pack, unpack, replace)
convertToInt :: [[Char]] -> [Int]
convertToInt = map read
getCoordinateNumbers :: [Char] -> [Char]
getCoordinateNumbers =  filter (`elem` ['0'..'9'])

getCoordinate :: [Char] -> [Char]
getCoordinate s = [head s, last s]
filterCoordinates :: [[Char]] -> [[Char]]
filterCoordinates = map getCoordinate
filterInputs :: [[Char]] -> [[Char]]
filterInputs = map getCoordinateNumbers


replaceSubstring :: String -> String -> String -> String
replaceSubstring old new str = unpack $ replace (pack old) (pack new) (pack str)


one = replaceSubstring "one" "o1e"
two = replaceSubstring "two" "t2o"
three = replaceSubstring "three" "t3e"
four = replaceSubstring "four" "f4r"
five = replaceSubstring "five" "f5e"
six = replaceSubstring "six" "s6x"
seven = replaceSubstring "seven" "s7n"
eight = replaceSubstring "eight" "e8t"
nine = replaceSubstring "nine" "n9e"

convertWordsToInts :: String -> String
convertWordsToInts s = one $ two $ three $ four $ five $ six $ seven $ eight $ nine s

mapWordsToInts :: [String] -> [String]
mapWordsToInts = map convertWordsToInts

main :: IO ()
main = do
    file <- readFile "input.txt"
    print $ sum $ convertToInt $ filterCoordinates $ filterInputs $ mapWordsToInts $ lines file