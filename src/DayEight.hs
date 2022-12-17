{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE NoFieldSelectors #-}

module DayEight (main) where

import Data.Array
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.IO qualified as Text.IO
import Utils (tReadMaybe)
import Utils.Grid2d (Grid (..), Index (..), gridFrom2dList, gridIndex, safeAt)

type Tree = Int

data Direction = North | East | South | West
  deriving stock (Show)

data Treeple = Treeple Index Tree Bool

visible :: Treeple -> Bool
visible (Treeple _ _ b) = b

cardinals :: [Direction]
cardinals = [North, East, South, West]

type Forest = Grid Index Tree

treeAt :: Forest -> Index -> Maybe Tree
treeAt forest idx = forest.grid `safeAt` gridIndex forest idx

forestFromList :: [[Int]] -> Forest
forestFromList = gridFrom2dList

getForestIndices :: Forest -> [Index]
getForestIndices g = range (Index 0 0, maxIndex g)

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
  let arrayIndexes = gridIndex forest <$> goDirection (maxIndex forest) idx dir
   in mapMaybe (forest.grid `safeAt`) arrayIndexes

goDirection :: Index -> Index -> Direction -> [Index]
goDirection (Index h w) (Index y x) dir = case dir of
  North -> [Index y' x | y' <- [y, y - 1 .. 0]]
  East -> [Index y x' | x' <- [x .. w - 1]]
  South -> [Index y' x | y' <- [y .. h - 1]]
  West -> [Index y x' | x' <- [x, x - 1 .. 0]]

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
