{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE NoFieldSelectors #-}

module DayEight (main) where

import Data.Array
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.IO qualified as Text.IO
import Utils (tReadMaybe)

type Tree = Int

type Index = (Int, Int)

data Direction = North | East | South | West
  deriving stock (Show)

data Treeple = Treeple Index Tree Bool

visible :: Treeple -> Bool
visible (Treeple _ _ b) = b

cardinals :: [Direction]
cardinals = [North, East, South, West]

fromIndex :: Int -> Index -> Int
fromIndex width (y, x) = (y * width) + x

data Forest = Forest {trees :: Array Int Tree, width :: Int, height :: Int}
  deriving stock (Show)

safeAt :: Ix i => Array i e -> i -> Maybe e
safeAt arr ix = if inRange (bounds arr) ix then Just (arr ! ix) else Nothing

treeAt :: Forest -> Index -> Maybe Tree
treeAt forest idx = forest.trees `safeAt` fromIndex forest.height idx

forestFromList :: [[Int]] -> Forest
forestFromList xss = Forest trees w h
  where
    trees = listArray (0, (w * h) - 1) (concat xss)
    w = length $ head xss
    h = length xss

getForestIndices :: Forest -> [Index]
getForestIndices (Forest _ w h) = [(x, y) | x <- [0 .. w - 1], y <- [0 .. h - 1]]

-- find out if tree is visible from any of north, west, south, east
treeIsVisible :: Forest -> Index -> Bool
treeIsVisible forest idx = any (treeIsVisibleBetween . getToEdge idx forest) cardinals

treeIsVisibleBetween :: [Tree] -> Bool
treeIsVisibleBetween trees = not $ any (>= head trees) (tail trees)

treesVisibleFrom :: Forest -> Index -> [[Tree]]
treesVisibleFrom forest idx = map (takeUntilViewBlocked . getToEdge idx forest) cardinals

takeUntilViewBlocked :: [Tree] -> [Tree]
takeUntilViewBlocked trees = takeWhile (< head trees) (tail trees) ++ blockingTree
  where
    blockingTree = take 1 $ dropWhile (< head trees) (tail trees)

scenicScore :: Forest -> Index -> Int
scenicScore forest = product . fmap length . treesVisibleFrom forest

filterVisisbleTrees :: Forest -> [Treeple]
filterVisisbleTrees forest =
  let toTreeple i = forest `treeAt` i >>= \tree -> pure $ Treeple i tree (treeIsVisible forest i)
      theTrees = mapMaybe toTreeple (getForestIndices forest)
   in filter visible theTrees

getToEdge :: Index -> Forest -> Direction -> [Tree]
getToEdge idx forest dir =
  let arrayIndexes = fromIndex forest.height <$> goDirection idx
   in mapMaybe (forest.trees `safeAt`) arrayIndexes
  where
    goDirection = case dir of
      North -> \(y, x) -> [(y', x) | y' <- [y, y - 1 .. 0]]
      East -> \(y, x) -> [(y, x') | x' <- [x .. forest.width - 1]]
      South -> \(y, x) -> [(y', x) | y' <- [y .. forest.height - 1]]
      West -> \(y, x) -> [(y, x') | x' <- [x, x - 1 .. 0]]

treesListFile :: IO Text
treesListFile = Text.IO.readFile "./files/DayEight/DayEight.txt"

main :: IO ()
main = do
  treesList <- treesListFile
  let treeChars = Text.chunksOf 1 <$> Text.splitOn "\n" (Text.stripEnd treesList)
      trees = mapMaybe tReadMaybe <$> treeChars :: [[Int]]
      forest = forestFromList trees
      visibleTreeCount = length $ filterVisisbleTrees forest
  putStrLn $ "QA: " <> show visibleTreeCount
  let mostScenicTreeScore = maximum $ fmap (scenicScore forest) (getForestIndices forest)
  putStrLn $ "QB: " <> show mostScenicTreeScore
