module PHSLib where

import Data.List (elemIndex, find, groupBy, intercalate, intersperse, nub, union)
import Data.Map (Map, elems, empty, fromList, insert, keys, lookup, member, singleton, toList)
import System.Console.ANSI
import System.Directory (getCurrentDirectory, getDirectoryContents)
import System.Environment (getArgs)
import System.FilePath (takeExtension)
import System.Random (Random, RandomGen, getStdGen, randomRIO, randomRs, randoms)
import Text.Read (readMaybe)
import Prelude hiding (Word, empty, insert, lookup)

-- Types

data Item = Chapter Int String | Section Item Int String | Page Int deriving (Eq, Ord)

instance Show Item where
  show (Chapter n s) = 'c' : show n ++ " : " ++ s
  show (Page n) = 'p' : show n
  show (Section (Chapter cn _) n s) = 's' : show cn ++ '.' : show n ++ " : " ++ s

data Word = Word [String] [String] deriving (Show, Eq)

data Token = TChapter | TPage | TSection | TString String | TEq | TOr | TWord deriving (Show, Eq)

type WordMap = Map Item [Word]

type ItemMap = (Map Item [Item], [Either Int (Int, Int)])

-- Helper functions

delnth :: Eq a => [Maybe a] -> [a]
delnth = map (\(Just x) -> x) . filter (/= Nothing)

splitWhen :: (a -> Bool) -> [a] -> ([a], [a])
splitWhen p xx@(x : xs)
  | p x = ([], xx)
  | otherwise =
    let (rl, rr) = splitWhen p xs
     in (x : rl, rr)
splitWhen p [] = ([], [])

insertInMapOfList :: Ord k => k -> v -> Map k [v] -> Map k [v]
insertInMapOfList i w wordmap =
  case lookup i wordmap of
    Just words -> insert i (w : words) wordmap
    Nothing -> insert i [w] wordmap

groupNums :: (Eq a, Enum a) => [a] -> [Either a (a, a)]
groupNums l =
  gn Nothing l
  where
    gn first@(Just f) (n1 : nss@(n2 : ns))
      | n2 == succ n1 = gn first $ nss
      | otherwise = Right (f, n1) : gn Nothing nss
    gn Nothing (n1 : nss@(n2 : ns))
      | n2 == succ n1 = gn (Just n1) $ nss
      | otherwise = Left n1 : gn Nothing nss
    gn (Just f) (n1 : []) = [Right (f, n1)]
    gn Nothing (n1 : []) = [Left n1]
    gn _ [] = []

withColor :: IO a -> (ColorIntensity, Color) -> IO ()
withColor m (i, c) = do
  setSGR [SetColor Foreground i c]
  m
  setSGR [SetDefaultColor Foreground]

-- Parsing database

keyWords :: Map String Token
keyWords = fromList [("#chapter", TChapter), ("#page", TPage), ("#section", TSection), ("=", TEq), ("|", TOr), ("&", TWord)]

tokenizeLineWords :: [String] -> [Token]
tokenizeLineWords ww@(w : ws) =
  case lookup w keyWords of
    Just x -> x : tokenizeLineWords ws
    Nothing ->
      let (s, r) = splitWhen (`member` keyWords) ww
       in TString (unwords s) : tokenizeLineWords r
tokenizeLineWords [] = []

getWord :: [Token] -> Word
getWord tks =
  let (en, eq : fr) = splitWhen (== TEq) tks
   in Word (remor en) (remor fr)
  where
    remor = map (\(TString s) -> s) . filter (/= TOr)

parseWordMap :: [Token] -> WordMap
parseWordMap tokens =
  pwm Nothing Nothing Nothing tokens
  where
    pwm _ p s (TChapter : TString n : TEq : TString ch : ts) = pwm (Just (read n, ch)) p s ts
    pwm c _ s (TPage : TString n : ts) = pwm c (Just $ read n) s ts
    pwm c@(Just (cn, ch)) p _ (TSection : TString n : TEq : TString s : ts) = pwm c p (Just (Chapter cn ch, read n, s)) ts
    pwm c p s (TWord : ts) =
      let (welems, rest) = splitWhen (`elem` [TChapter, TPage, TSection, TWord]) ts
       in insertWordWithItems c p s (getWord welems) $ pwm c p s rest
    pwm _ _ _ _ = empty

insertWordWithItems :: Maybe (Int, String) -> Maybe Int -> Maybe (Item, Int, String) -> Word -> WordMap -> WordMap
insertWordWithItems (Just (c, ch)) p s w wm = insertInMapOfList (Chapter c ch) w $ insertWordWithItems Nothing p s w wm
insertWordWithItems _ (Just p) s w wm = insertInMapOfList (Page p) w $ insertWordWithItems Nothing Nothing s w wm
insertWordWithItems _ _ (Just (cn, n, s)) w wm = insertInMapOfList (Section cn n s) w wm
insertWordWithItems _ _ _ _ wm = wm

-- Parsing user input

parseChoice :: [String] -> ([Int], [(Int, Int)], [Int])
parseChoice (('c' : cs) : ws) =
  let (cn, sn, pn) = parseChoice ws
   in (getNums cs ++ cn, sn, pn)
parseChoice (('s' : cs) : ws) =
  let (cn, sn, pn) = parseChoice ws
   in (cn, getSecNums cs ++ sn, pn)
parseChoice (('p' : cs) : ws) =
  let (cn, sn, pn) = parseChoice ws
   in (cn, sn, getNums cs ++ pn)
parseChoice (_ : ws) = parseChoice ws
parseChoice [] = ([], [], [])

getNums :: String -> [Int]
getNums s =
  case readMaybe s of
    Just n -> [n]
    Nothing ->
      let (s1, m : s2) = splitWhen (== '-') s
       in [read s1 .. read s2]

getSecNums :: String -> [(Int, Int)]
getSecNums s =
  let (s1, p : s2) = splitWhen (== '.') s
      c = read s1
   in map (\n -> (c, n)) $ getNums s2

getItems :: ([Int], [(Int, Int)], [Int]) -> WordMap -> [Item]
getItems (cns, sns, pns) wm =
  delnth $
    map (\n -> find (isChap n) $ keys wm) cns
      ++ map (\(c, n) -> find (isSec c n) $ keys wm) sns
      ++ map (\n -> find (isPage n) $ keys wm) pns
  where
    isChap n (Chapter c _) = c == n
    isChap _ _ = False
    isSec c n (Section (Chapter ch _) s _) = c == ch && n == s
    isSec _ _ _ = False
    isPage n (Page p) = p == n
    isPage _ _ = False

parseUserChoice :: WordMap -> String -> [Word]
parseUserChoice wmap choice =
  let chosennums = parseChoice $ words choice
      chosenitems = getItems chosennums wmap
      chosenwords = concat . delnth $ map (`lookup` wmap) chosenitems
   in chosenwords

-- Items transformations

scanItems :: [Item] -> ItemMap
scanItems (i@(Section c _ _) : is) =
  let (m, ps) = scanItems is
   in (insertInMapOfList c i m, ps)
scanItems iss@((Page n) : is) =
  (empty, groupNums . map (\(Page n) -> n) $ iss)
scanItems (_ : is) = scanItems is
scanItems [] = (empty, [])

showItems :: ItemMap -> String
showItems (m, ps) =
  (unlines $ concat $ map (\(c, ss) -> show c : (map (\s -> " - " ++ show s) ss)) $ toList m)
    ++ (unwords $ map ungn ps)
  where
    ungn (Left n) = 'p' : show n
    ungn (Right (n1, n2)) = 'p' : show n1 ++ "-" ++ show n2

-- Random

getModes :: RandomGen g => g -> String -> Int -> [Bool]
getModes gen "1" l = repeat True
getModes gen "2" l = repeat False
getModes gen "3" l =
  let bs = replicate (floor $ fromIntegral l / 2) True ++ replicate (ceiling $ fromIntegral l / 2) False
      rns = randomRs (0, l -1) gen
   in map (bs !!) . nub $ rns
getModes gen "4" l = take l $ randoms gen

getRandomWords :: RandomGen g => g -> [Word] -> String -> [Word]
getRandomWords gen chosenwords maxnum =
  let l = length chosenwords
      m = if maxnum == "all" then maxBound else read maxnum
      rns = randomRs (0, l -1) gen
   in take (min m l) . map (chosenwords !!) . nub $ rns

atRandIndex :: [a] -> IO a
atRandIndex l = do
  i <- randomRIO (0, length l - 1)
  return $ l !! i

-- Prompting user

getDatabaseContents :: IO (WordMap, ItemMap)
getDatabaseContents = do
  files <- (getCurrentDirectory >>= getDirectoryContents)
  contents <- mapM readFile $ filter ((== ".pehaut") . takeExtension) files
  let tokens = concatMap (tokenizeLineWords . words) contents
      wmap = parseWordMap tokens
      items = scanItems $ keys wmap
  return (wmap, items)

displayContents :: ItemMap -> IO ()
displayContents items = do
  putStrLn "Hi, here are the avaliable contents :"
  putStrLn $ showItems items

promptMode :: IO String
promptMode = do
  putStrLn "In which mode do you want to run ?"
  putStrLn "1 : Fr -> En"
  putStrLn "2 : En -> Fr"
  putStrLn "3 : Fr <-> En, 50/50"
  putStrLn "4 : Fr <-> En, Random"
  getLine

promptMax :: IO String
promptMax = do
  putStrLn "Max number of words ('all' for testing all the words of the selected parts) :"
  getLine

promptChoice :: p -> IO String
promptChoice wmap = do
  putStrLn "Which should be included ? (separated by a space)"
  getLine

promptEnd :: IO String
promptEnd = do
  putStrLn "R : recommencer la série    N : recommencer avec une nouvelle liste de mots aléatoire    C : retour aux paramètres de la série"
  getLine

testWord :: (Word, Bool) -> IO Bool
testWord (Word enws frws, b) = do
  let (l1, l2) = if b then (frws, enws) else (enws, frws)
  w1 <- atRandIndex l1
  if b
    then do
      putStr $ "What's the english for "
      putStr w1 `withColor` (Vivid, Blue)
      putStrLn " ?"
    else do
      putStr $ "Comment dit-on "
      putStr w1 `withColor` (Vivid, Blue)
      putStrLn " en français ?"
  w2 <- getLine
  let ok = w2 `elem` l2
  if ok
    then do
      putStrLn "Correct !" `withColor` (Vivid, Green)
    else do
      putStrLn "Nooo god !" `withColor` (Vivid, Red)
      putStr "The correct answers were : "
      sequence_ . intersperse (putStr ", or, ") . map (\s -> putStr s `withColor` (Vivid, Cyan)) $ l2
      putStrLn ""
  return ok

runTest :: [(Word, Bool)] -> IO String
runTest ws = do
  res <- mapM testWord ws
  let l = length ws
      score = length . filter id $ res
      rat = 100 * fromIntegral score / fromIntegral l
  putStrLn (show score ++ "/" ++ show l ++ " soit " ++ show (round rat) ++ "%") `withColor` (Vivid, Magenta)
  promptEnd
