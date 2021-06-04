{-# LANGUAGE GADTs #-}

module Main where

import System.IO
import System.Environment

import Data.List
import Data.Function
import Data.Bool
import qualified Data.Map as Map (insert)
import Data.Map (Map, fromList, toList, singleton, unionWith, findWithDefault)
import Control.Applicative
import Text.Printf

run :: Eq a => (Int -> a -> b) -> [a] -> [b]
run f []     = []
run f (x:xs) =
  let (l,r) = span (x ==) xs
      len   = 1 + length l
      rest  = run f r
  in f len x : rest

runs :: Eq a => [a] -> [Int]
runs = run const

run_length :: Eq a => [a] -> [(Int,a)]
run_length = run (,)

(//) :: (Integral a, Fractional b) => a -> a -> b
(//) = (/) `on` fromIntegral

entropy' :: Ord a => [a] -> Float
entropy' xs =
  let len = length xs
      op  = (*) <*> logBase 2
  in negate $ sum $ op . (// len) <$> (runs $ sort xs)

entropy :: Ord a => [[a]] -> Float
entropy xss = entropy' $ last <$> xss

-- This is actually (ig(D,x) - E(D))
ig :: Ord a => [[a]] -> Int -> Float
ig d x =
  let dxv v   = filter ((v ==) . (!! x)) d
      op  dxv = ((//) `on` length) dxv d * entropy dxv
  in -(sum $ (op . dxv) <$> nub ((!! x) <$> d))


ig' :: Ord a => [[a]] -> Int -> Float
ig' d x =
  entropy d - s
  where dxv v  = filter ((v ==) . (!! x)) d
        op dxv = ((//) `on` length) dxv d * entropy dxv
        s      = sum $ op . dxv <$> (nub $ (!! x) <$> d)

split' :: (a -> Bool) -> [a] -> ([a],[a])
split' _ [] = ([],[])
split' p (x:xs)
  | p x       = (xs,[])
  | otherwise = (x:) <$> split' p xs

split :: (a -> Bool) -> [a] -> [[a]]
split _ [] = []
split p xs = y : split p rest
  where (rest,y) = split' p xs

type Dataset = [[String]]

data Tre a where
  Node :: Int -> (a, Map a (Tre a)) -> Tre a
  Leaf :: a   -> Tre a
instance Show a => Show (Tre a) where
  show (Leaf a)          = show a
  show (Node i (def, m)) = "(" ++ show i ++ "(" ++ show def ++ "), " ++ showMap m ++ ")"

predictSingle :: Eq a => Tre a -> [a] -> a
predictSingle (Leaf a)          _ = a
predictSingle (Node n (def, m)) x =
  let v = ((lookup (x !! n)) . toList) m
  in case v of
    Just c  -> predictSingle c x
    Nothing -> def

type Tree = Tre String

argmax :: Ord b => (a -> a -> Ordering) -> (a -> b) -> [a] -> a
argmax g f a = maximumBy (\a b -> f a `compare` f b <> b `g` a) a

most_common' :: Ord a => [a] -> a
most_common' = snd . head . sortBy comparison . run_length . sort
               where comparison (a,b) (c,d) = c `compare` a <> b `compare` d

most_common :: Ord a => [[a]] -> a
most_common d = most_common' $ last <$> d

id3' :: Dataset -> Dataset -> [Int] -> [String] -> Int -> Tree
id3' [] dparent xs names depth = Leaf $ most_common dparent
id3' d  dparent xs names 0     = Leaf $ most_common d
id3' d  dparent [] names depth = Leaf $ most_common d
id3' d  dparent xs names depth =
  let mc = most_common d
  in if length d == length (filter ((mc ==) . last) d)
     then Leaf mc
     else
       let cmp  = compare `on` (names !!)
           x    = argmax cmp (ig d) xs
           xs'  = filter (/= x) xs
           dxs  = (!! x) <$> d
           op v = id3' (filter ((v ==) . (!! x)) d) d xs' names (depth-1)
           sub  = fromList $ (,) <*> op <$> dxs
       in Node x (mc,sub)

id3 :: Dataset -> [String] -> Int -> Tree
id3 d = id3' d d [0..((length $ head d)-2)]

confusionMatrix :: [String] -> [String] -> Map (String,String) Int
confusionMatrix []     []     = mempty
confusionMatrix (a:as) (b:bs) = unionWith (+) (singleton (a,b) 1) (confusionMatrix as bs)

fullMatrix :: Map (String,String) Int -> [(String,String)] -> [Int]
fullMatrix m = (flip (findWithDefault 0) m <$>)

show_kv :: (Show k, Show v) => (k,v) -> String
show_kv (k,v) = (show k) ++ ": " ++ (show v) ++ ", "

showMap :: (Show k, Show v) => (Map k v) -> String
showMap x =
  "{" ++ (concat $ show_kv <$> l) ++ "}"
  where l = (toList x)

showTree'' :: String -> Int -> [String] -> (Map String Tree) -> String
showTree'' x n names m =
  concat $ (\(k,v) -> showTree' (x++k++" ") n names v) <$> toList m

showTree' :: String -> Int -> [String] -> Tree -> String
showTree' x n names (Leaf a)          = x ++ a ++ "\n"
showTree' x n names (Node i (def, m)) =
  showTree'' (x ++ (show n) ++ ":" ++ (names !! i) ++ "=") (n+1) names m

showTree :: [String] -> Tree -> String
showTree = showTree' [] 1

-- n*n matrix
showMatrix' :: Int -> [Int] -> Int -> String
showMatrix' at matrix n =
  if at == n*n
  then ""
  else (if (at `mod` n == 0) then "\n" else " ") ++
       show (matrix !! at) ++
       showMatrix' (at + 1) matrix n

showMatrix :: [Int] -> Int -> String
showMatrix = showMatrix' 0

getlines :: Handle -> IO [String]
getlines = (fmap lines) . hGetContents

parseCSV :: String -> [String]
parseCSV = split (== ',')

parseFile :: [String] -> [[String]]
parseFile = fmap parseCSV

main :: IO ()
main = do
  args <- getArgs
  let (f0,f1,depth) = case length args of
                        0 -> ("train.csv", "test.csv", -1)
                        1 -> ("train.csv", "test.csv", read (args !! 0))
                        2 -> (args !! 0, args !! 1, -1)
                        3 -> (args !! 0, args !! 1, read (args !! 2))
  withFile f0 ReadMode $ \train -> do
    (n0:d0) <- getlines train
    let names = parseCSV n0
        tree  = id3 (parseFile d0) names depth
    putStrLn "[BRANCHES]:"
    putStr $ showTree names tree
    --putStrLn $ show tree
    withFile f1 ReadMode $ \test -> do
      (n1:d1) <- getlines test
      putStr "[PREDICTIONS]: "
      let parsed_d1   = parseFile d1
          predictions = predictSingle tree <$> parsed_d1
          actual_data = (last <$> parsed_d1)
          matches     = zipWith (==) actual_data predictions
          matrix      = confusionMatrix actual_data predictions
          elems       = sort $ nub (actual_data ++ predictions)
      putStrLn $ foldl' (\a b -> a ++ b ++ " ") [] predictions
      putStrLn $ "[ACCURACY]: " ++ printf "%.5f" ((length (filter (== True) matches) // length matches) :: Float)
      putStrLn $ "[CONFUSION_MATRIX]:" ++
                 (showMatrix (fullMatrix matrix [(x,y) | x <- elems, y <- elems]) $ length elems)
