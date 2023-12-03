module AdventOfCode (dig, splitValuesAndSymbols, isSchematic, possibleSchematics, sumCoordinateValues, filterToPotentialGears, filterGearRatios, sumAllGears) where
type Coordinates = (Int, Int, String)
type CoordinatesValue = (Int, Int, Int)
type CoordinatesSymbol = Coordinates

isNumeric :: String -> Bool
isNumeric str = case reads str :: [(Double, String)] of
               [(_, "")] -> True
               _         -> False

dig :: String -> Int -> String -> [Coordinates] -> [Coordinates]

invalidChar :: Char -> Bool
invalidChar c = c == '.' || c == '\n' || c == '\r'
isNewLine :: Char -> Bool
isNewLine c = c == '\n' || c == '\r'
isNumberChar :: Char -> Bool
isNumberChar i = i `elem` ['0'..'9']

dig (x : xs) i n coords
  | isNewLine x && null xs = coords
  | isNewLine x && not (null xs) && not (null n) = dig xs i "" coords ++ [(i - length n, length n, n)]
  | isNewLine x && not (null xs) && null n = dig xs i "" coords
  | invalidChar x && null xs = coords
  | invalidChar x && not (null xs) && not (null n) = dig xs (i+1) "" coords ++ [(i - length n, length n, n)]
  | invalidChar x && not (null xs) && null n = dig xs (i+1) "" coords
  | isNumberChar x && null xs = coords ++ [(i - length n, length n, n)]
  | isNumberChar x && not (null xs) = dig xs (i+1) (n++[x]) coords
  | not (isNumberChar x) && not (null xs) && not (null n) = dig xs (i+1) "" coords ++ [(i - length n, length n, n), (i, 1, [x])]
  | not (isNumberChar x) && not (null xs) = dig xs (i+1) "" coords ++ [(i, 1, [x])]
  | not (isNumberChar x) && null xs = coords
  | otherwise = coords

splitValuesAndSymbols :: [Coordinates] -> [CoordinatesValue] -> [CoordinatesSymbol] -> ([CoordinatesValue], [CoordinatesSymbol])
splitValuesAndSymbols ((i, l, value) : xs) values symbols
    | isNumeric value && null xs = (values ++ [(i, l, read value :: Int)], symbols)
    | isNumeric value = splitValuesAndSymbols xs (values ++ [(i,l, read value :: Int)]) symbols
    | not (isNumeric value) && null xs = (values, symbols ++ [(i, l, value)])
    | not (isNumeric value) = splitValuesAndSymbols xs values (symbols ++ [(i,l,value)])

    | otherwise = (values, symbols)


isSchematic :: [CoordinatesSymbol] -> Int -> CoordinatesValue -> Bool
isSchematic ((xPosition,_,_): xs) lineLength (lSide, cLength, value)
    | xPosition == leftSide = True
    | xPosition == rightSide = True

    | xPosition >= topLeft && xPosition <= topRight = True
    | xPosition >= bottomLeft && xPosition <= bottomRight = True

    | xPosition > bottomRight = False
    | not (null xs) = isSchematic xs lineLength (lSide, cLength, value)
    | otherwise = False

    where leftSide = lSide - 1
          rightSide = lSide + cLength
          topLeft = leftSide - lineLength
          topRight = rightSide - lineLength
          bottomRight = rightSide + lineLength
          bottomLeft = leftSide + lineLength



possibleSchematics :: [CoordinatesSymbol] -> Int -> [CoordinatesValue] -> [CoordinatesValue]
possibleSchematics symbols fileLength = filter (isSchematic symbols fileLength)

sumCoordinateValues :: [CoordinatesValue] -> Int
sumCoordinateValues values = sum (map (\(_, _, value) -> value ) values)


isGear :: CoordinatesSymbol -> Bool
isGear (_, _, value)= value == "*"
filterToPotentialGears :: [CoordinatesSymbol] -> [CoordinatesSymbol]
filterToPotentialGears = filter isGear

findGearRatios :: [CoordinatesValue] -> Int -> CoordinatesSymbol  -> (CoordinatesSymbol, [CoordinatesValue])
findGearRatios values lineLength x = (x, possibleSchematics [x] lineLength values)

mapGears :: [CoordinatesSymbol] -> [CoordinatesValue] -> Int  -> [(CoordinatesSymbol, [CoordinatesValue])]
mapGears gears values lineLength = map (findGearRatios values lineLength) gears
isValidGear :: (CoordinatesSymbol, [CoordinatesValue]) -> Bool
isValidGear (_, values) = length values == 2
filterGearRatios :: [CoordinatesSymbol] -> [CoordinatesValue] -> Int -> [(CoordinatesSymbol, [CoordinatesValue])]
filterGearRatios gears values lineLength = filter isValidGear (mapGears gears values lineLength)
calcRatio :: (CoordinatesSymbol, [CoordinatesValue]) -> Int
calcRatio (_, (_, _, x) : (_,_,y): xs) = x * y
mapGearRatios :: [(CoordinatesSymbol, [CoordinatesValue])] -> [Int]
mapGearRatios = map calcRatio
sumAllGears :: [(CoordinatesSymbol, [CoordinatesValue])] -> Int
sumAllGears values = sum (mapGearRatios values)
