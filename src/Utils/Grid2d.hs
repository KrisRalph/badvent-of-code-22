module Utils.Grid2d
  ( Index (..),
    Grid (..),
    safeAt,
    gridAt,
    gridIndex,
    gridIndexFromWidth,
    emptyGrid,
    gridFrom2dList,
  )
where

import Data.Array (Array, array, bounds, inRange, listArray, (!))
import Data.Array qualified as Array
import Data.Ix (Ix)

-- 2d indexing
data Index = Index Int Int
  deriving stock Show

instance Eq Index where
  (Index y x) == (Index b a) = y == b && x == a

instance Ord Index where
  (Index y x) <= (Index b a) = if y == b then x <= a else y <= b

instance Ix Index where
  range = index2dRange
  inRange = index2dInRange
  index = index2d

index2dRange :: (Index, Index) -> [Index]
index2dRange (Index lx ly, Index hx hy) =
  let xs = [lx .. hx - 1]
      ys = [ly .. hy - 1]
   in [Index x y | x <- xs, y <- ys]

index2d :: (Index, Index) -> Index -> Int
index2d (Index _ lx, Index _ hx) (Index y x) = let width' = hx - lx in (y * width') + x

index2dInRange :: (Index, Index) -> Index -> Bool
index2dInRange (Index ly lx, Index hy hx) (Index y x) = (y >= ly && y <= hy) && (x >= lx && x <= hx)

safeAt :: Ix i => Array i a -> i -> Maybe a
safeAt arr ix = if inRange (bounds arr) ix then Just (arr ! ix) else Nothing

-- grids
data Grid i a = Grid {grid :: Array Int a, maxIndex :: i}
  deriving stock (Show)

gridAt :: Grid Index a -> Index -> Maybe a
gridAt g i = grid g `safeAt` gridIndex g i

gridIndex :: Grid Index a -> Index -> Int
gridIndex g = gridIndexFromWidth (width g)

width :: Grid Index a -> Int
width (Grid _ (Index _ x)) = x

gridIndexFromWidth :: Int -> Index -> Int
gridIndexFromWidth maxW (Index y x) = (y * maxW) + x

emptyGrid :: Int -> Int -> a -> Grid Index a
emptyGrid w h filler = Grid {grid = newGrid, maxIndex = Index w h}
  where
    newGrid = array (0, (w * h) - 1) (zip [0 ..] (repeat filler))

gridFrom2dList :: [[a]] -> Grid Index a
gridFrom2dList xss = Grid newArray (Index w h)
  where
    newArray = listArray (0, (w * h) - 1) (concat xss)
    w = length $ head xss
    h = length xss
