module Main where
import AdventOfCode (dig, splitValuesAndSymbols, possibleSchematics, isSchematic, sumCoordinateValues, )

printTestData = do
    testFile <- readFile "test.txt"
    let testCoords = reverse (dig testFile 0 "" [])
    print testCoords
    let (testValues, testSymbols) = splitValuesAndSymbols testCoords [] []
    print testValues
    print testSymbols
    let testFiltered = possibleSchematics testCoords 10 testValues
    print testFiltered
    let testSum = sumCoordinateValues testFiltered
    print (testSum, testSum == 4361)

printRealData = do
    testFile <- readFile "input.txt"
    let testCoords = reverse (dig testFile 0 "" [])
    print testCoords
    let (testValues, testSymbols) = splitValuesAndSymbols testCoords [] []
    print "TEST VALUES#################################################################"
    print testValues
    print "TEST COORDS#################################################################"
    print testSymbols
    print "END COORDS#################################################################"
    let testFiltered = possibleSchematics testCoords 140 testValues
    print testFiltered
    let testSum = sumCoordinateValues testFiltered
    print testSum

main :: IO ()
main = do
    printRealData
    printTestData
