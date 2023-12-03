{-# LANGUAGE OverloadedStrings #-}
import Data.Text (splitOn, pack, unpack, replace, strip)
import GHC.Real (reduce)

type ColorPair = (Int, String)
type RGB = (Int, Int, Int)

data Game = Game {
    gameId :: Int,
    -- rgb :: [RGB]
    rgb :: RGB
} deriving (Show)

trim :: String -> String
trim str = unpack $ strip (pack str)

replaceSubstring :: String -> String -> String -> String
replaceSubstring old new str = unpack $ replace (pack old) (pack new) (pack str)

splitOnColon s = map unpack (splitOn ":" (pack s))
splitOnSemi s = map unpack (splitOn ";" (pack s))
splitOnComma s = map unpack (splitOn "," (pack s))

addColorByLabel :: RGB -> ColorPair -> RGB
addColorByLabel (r,g,b) (color,label) = case label of
    "green" -> (r,g + color, b)
    "red" -> (r+color, g,b)
    "blue" -> (r,g,b+color)
    _ -> (r,g,b)

maxColorByLabel :: RGB -> ColorPair -> RGB    
maxColorByLabel (r,g,b) (color, label) 
    | label == "green" && color > g = (r,color, b)
    | label == "blue" && color > b = (r, g, color) 
    | label == "red" && color > r = (color, g, b)    
    | otherwise = (r,g,b)

foldColors :: RGB -> [ColorPair] -> RGB
foldColors z [] = z
foldColors z (x:xs) = foldColors (maxColorByLabel z x) xs
-- foldColors z (x:xs) = foldColors (addColorByLabel z x) xs


removeGameText :: String -> String
removeGameText = replaceSubstring "Game " ""
convertToInt :: [Char] -> Int
convertToInt = read

stringListToTuple :: [String] -> ColorPair
stringListToTuple s = (convertToInt $ head s, last s)

mapStringListToTuples :: [[String]] -> [ColorPair]
mapStringListToTuples = map stringListToTuple

anotherMap :: [String] -> [[String]]
anotherMap = map words
colorStringToTuple :: String -> [ColorPair]
colorStringToTuple  =  mapStringListToTuples . anotherMap . splitOnComma . trim

mapRoundsToTuples :: String -> [[ColorPair]]
mapRoundsToTuples = map colorStringToTuple . splitOnSemi

mapColorTuplestoRGB :: [ColorPair] -> RGB
mapColorTuplestoRGB = foldColors (0,0,0)
mapRoundsOfColors :: [[ColorPair]] -> [RGB]
mapRoundsOfColors = map mapColorTuplestoRGB

-- convertGameTextLines :: [String] -> Game
-- convertGameTextLines (x: xs) = Game (read $ removeGameText x)  (mapRoundsOfColors . mapRoundsToTuples $ head xs)

convertGameTextLines :: [String] -> Game
convertGameTextLines (x: xs) = Game (read $ removeGameText x)  (mapColorTuplestoRGB . concat . mapRoundsToTuples $ head xs)


convertToGame :: String -> Game
convertToGame s = convertGameTextLines $ splitOnColon s

-- maxRed = 12
-- maxGreen = 13
-- maxBlue = 14
rgbWithinLimit :: (Ord a1, Ord a2, Ord a3, Num a1, Num a2, Num a3) => (a1, a2, a3) -> Bool
rgbWithinLimit (r,g,b) = r <= 12 && g <= 13 && b <= 14

-- isGamePossible :: Game -> Bool
-- isGamePossible game = all rgbWithinLimit (rgb game)

rgbPower :: Num a => (a, a, a) -> a
rgbPower (r,g,b) = r * g * b
getGamePower :: Game -> Int
getGamePower game = rgbPower (rgb game)

main = do
    file <- readFile "input.txt"
    let games = map convertToGame $ lines file
    let powers = map getGamePower games
    -- let possibleGames = filter isGamePossible games
    -- let gameIds = map gameId possibleGames
    -- let value = sum gameIds
    print (sum powers)