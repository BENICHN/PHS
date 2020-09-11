module Main where

import Data.List (elemIndex)
import PHSLib
import System.Console.ANSI
import System.Environment (getArgs)
import System.Random (getStdGen)

main :: IO ()
main = do
  args <- getArgs
  gen <- getStdGen
  (wmap, items) <- getDatabaseContents
  if "--hide" `elem` args then return () else displayContents items
  choice <-
    case "--choice" `elemIndex` args of
      Just i -> return $ args !! (i + 1)
      Nothing -> promptChoice wmap
  mode <-
    case "--mode" `elemIndex` args of
      Just i -> return $ args !! (i + 1)
      Nothing -> promptMode
  maxnum <-
    case "--maxnum" `elemIndex` args of
      Just i -> return $ args !! (i + 1)
      Nothing -> promptMax
  putStrLn ("To make these choices again, you can run the program with the folowwing arguments : --hide --choice " ++ choice ++ " --mode " ++ mode ++ " --maxnum " ++ maxnum) `withColor` (Vivid, Yellow)
  let chosenwords = parseUserChoice wmap choice
      wordstotest = getRandomWords gen chosenwords maxnum
      modes = getModes gen mode $ length wordstotest
      ws = zip wordstotest modes
  runTest ws
