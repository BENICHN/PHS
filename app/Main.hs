{-# LANGUAGE MultiWayIf #-}

module Main where

import Data.List (elemIndex)
import PHSLib
import System.Console.ANSI
import System.Environment (getArgs)
import System.Random (RandomGen, getStdGen)

main :: IO ()
main = main' False

getArg args arg prompt ignoreArgs =
  if ignoreArgs
    then prompt
    else case arg `elemIndex` args of
      Just i -> return $ args !! (i + 1)
      Nothing -> prompt

main' :: Bool -> IO ()
main' ignoreArgs = do
  args <- getArgs
  (wmap, count, items) <- getDatabaseContents
  if not ignoreArgs && "--hide" `elem` args then return () else displayContents items count
  choice <- getArg args "--choice" promptChoice ignoreArgs
  mode <- getArg args "--mode" promptMode ignoreArgs
  maxnum <- getArg args "--maxnum" promptMax ignoreArgs
  putStr "Pour refaire ces choix, exécutez le programme avec la commande suivante : "
  putStrLn ("--hide --choice " ++ choice ++ " --mode " ++ mode ++ " --maxnum " ++ maxnum) `withColor` (Vivid, Magenta)
  putStrLn "Appuyez sur entrer pour démarrer le test..."
  getLine
  start wmap choice mode maxnum

start :: WordMap -> String -> String -> String -> IO ()
start wmap choice mode maxnum = do
  gen <- getStdGen
  let chosenwords = parseUserChoice wmap choice
      wordstotest = getRandomWords gen chosenwords maxnum
      modes = getModes gen mode $ length wordstotest
      ws = zip wordstotest modes
  s <- runTests ws
  if
      | s `elem` ["N", "n"] -> start wmap choice mode maxnum
      | s `elem` ["C", "c"] -> main' True
      | otherwise -> return ()

runTests :: [WordTest] -> IO String
runTests ws = do
  rest <- runTest ws
  s <- promptEnd
  end s rest
  where
    end s rest
      | s `elem` ["R", "r"] = runTests ws
      | s `elem` ["X", "x"] = runTests rest
      | otherwise = return s