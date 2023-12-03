module Main where
import AdventOfCode (dig, splitValuesAndSymbols, possibleSchematics, isSchematic, sumCoordinateValues, filterToPotentialGears, filterGearRatios, sumAllGears, filterGearRatios)

testFile = do
    file <- readFile "test.txt"
    let testData = reverse (dig file 0 "" []) 
    let (values, symbols) = splitValuesAndSymbols testData [] []
    print values
    let potentialGears = filterToPotentialGears symbols
    let potentialGearValues = possibleSchematics potentialGears 10 values 
    print potentialGears
    print potentialGearValues
    let gears = filterGearRatios potentialGears potentialGearValues 10
    print (sumAllGears gears)

part2 = do
    file <- readFile "input.txt"
    let testData = reverse (dig file 0 "" []) 
    let (values, symbols) = splitValuesAndSymbols testData [] []    
    let potentialGears = filterToPotentialGears symbols
    let potentialGearValues = possibleSchematics potentialGears 140 values     
    let gears = filterGearRatios potentialGears potentialGearValues 140
    print (sumAllGears gears)    
    

main = do
    testFile
    part2
-- 75312571