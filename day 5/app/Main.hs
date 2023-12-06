module Main where
import AdventOfCode (ingestFile)

-- testFile = do
--     file <- readFile "test.txt"
--     let output = ingestFile file
--     print(35 == output, output)

aoc = do
    file <- readFile "input.txt"
    ingestFile file
    -- print(35 == output, output)
main :: IO ()
main = do
    -- testFile
    aoc
