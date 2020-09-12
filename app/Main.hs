module Main where

import Data.List (elemIndex)
import PHSLib
import System.Console.ANSI
import System.Environment (getArgs)
import System.Random (RandomGen, getStdGen)

main :: IO ()
main = main' False

main' :: Bool -> IO ()
main' ignoreArgs = do
  args <- getArgs
  gen <- getStdGen
  (wmap, items) <- getDatabaseContents
  if not ignoreArgs && "--hide" `elem` args then return () else displayContents items
  choice <-
    if ignoreArgs then promptChoice wmap
    else
      case "--choice" `elemIndex` args of
        Just i -> return $ args !! (i + 1)
        Nothing -> promptChoice wmap
  mode <-
    if ignoreArgs then promptMode
    else
      case "--mode" `elemIndex` args of
        Just i -> return $ args !! (i + 1)
        Nothing -> promptMode
  maxnum <-
    if ignoreArgs then promptMax
    else
      case "--maxnum" `elemIndex` args of
        Just i -> return $ args !! (i + 1)
        Nothing -> promptMax
  putStrLn ("To make these choices again, you can run the program with the folowwing arguments : --hide --choice " ++ choice ++ " --mode " ++ mode ++ " --maxnum " ++ maxnum) `withColor` (Vivid, Yellow)
  getLine
  start gen wmap choice mode maxnum

start :: RandomGen b => b -> WordMap -> String -> String -> String -> IO ()
start gen wmap choice mode maxnum =
  let chosenwords = parseUserChoice wmap choice
      wordstotest = getRandomWords gen chosenwords maxnum
      modes = getModes gen mode $ length wordstotest
      ws = zip wordstotest modes
  in runTest ws >>= end
  where
    end "R" = start gen wmap choice mode maxnum
    end "N" = getStdGen >>= \g -> start g wmap choice mode maxnum
    end "C" = main' True
    end _ = return ()