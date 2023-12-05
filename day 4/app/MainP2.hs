module Main where
import AdventOfCode (computeResultP2)

testFile = do
  file <- readFile "test.txt"
  let totalGames = computeResultP2 file
  print (totalGames == 30, totalGames)
part2File = do
  file <- readFile "input.txt"
  let totalGames = computeResultP2 file
  print (totalGames == 13768818, totalGames)

main :: IO ()
main = do
  testFile
  part2File