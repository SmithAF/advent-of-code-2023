module Main where
import AdventOfCode (computeResult)

computeTest :: IO Int
computeTest = do
  file <- readFile "test.txt"
  return (computeResult file)

main :: IO ()
main = do
  testResult <- computeTest
  print (13 == testResult)