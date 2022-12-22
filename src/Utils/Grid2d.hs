module Utils.Grid2d
  ( Index (..),
    Grid (..),
    safeAt,
    gridAt,
    gridSetAt,
    gridIndex,
    gridIndexFromWidth,
    gridIndexIsInBounds,
    gridIntToIndex,
    emptyGrid,
    gridFrom2dList,
    gridTo2dList,
    gridWidth,
    gridIndexOf,
    gridAllIndexesOf
  )
where

import Data.Array (Array, array, bounds, inRange, listArray, (!), (//), assocs)
import Data.Array qualified as Array
import Data.Ix (Ix)
import Utils (chunksOf)
import Data.List (find)
import GHC.Generics (Generic)
import Data.Hashable (Hashable, hash)

-- 2d indexing
data Index = Index Int Int
  deriving stock (Show, Generic)
  
-- this instance is somewhat bogus
instance Hashable Index where
  hash (Index y x) = y * x

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

gridIndexOf :: Eq a => Grid Index a -> a -> Maybe Index
gridIndexOf g ele =
  let found = find ((==ele) . snd) (assocs $ grid g)
  in gridIntToIndex g . fst <$> found

gridAllIndexesOf :: Eq a => Grid Index a -> a -> [Index]
gridAllIndexesOf g ele =
  let found = filter ((==ele) . snd) (assocs $ grid g)
  in gridIntToIndex g . fst <$> found

gridSetAt :: Grid Index a -> Index -> a -> Grid Index a
gridSetAt g i newVal = 
  let newAssocIndex = gridIndex g i
      newArray = grid g // [(newAssocIndex, newVal)]
  in g { grid = newArray }

gridIndex :: Grid Index a -> Index -> Int
gridIndex g = gridIndexFromWidth (gridWidth g)

gridIndexIsInBounds :: Grid Index a -> Index -> Bool
gridIndexIsInBounds g (Index y x) = 
  let (Index maxY maxX) = maxIndex g
  in (y < maxY && x < maxX && y >= 0 && x >= 0)

gridIntToIndex :: Grid Index a -> Int -> Index
gridIntToIndex g i = Index y x
  where (y, x) = i `quotRem` gridWidth g

gridWidth :: Grid Index a -> Int
gridWidth (Grid _ (Index _ x)) = x

gridIndexFromWidth :: Int -> Index -> Int
gridIndexFromWidth maxW (Index y x) = (y * maxW) + x

emptyGrid :: Int -> Int -> a -> Grid Index a
emptyGrid w h filler = Grid {grid = newGrid, maxIndex = Index w h}
  where
    newGrid = array (0, (w * h) - 1) (map (, filler) [0 ..])

gridFrom2dList :: [[a]] -> Grid Index a
gridFrom2dList xss = Grid newArray (Index h w)
  where
    newArray = listArray (0, (w * h) - 1) (concat xss)
    w = length $ head xss
    h = length xss

gridTo2dList :: Grid Index a -> [[a]]
gridTo2dList g = 
  let maxX = gridWidth g
      grid' = snd <$> assocs (grid g)
  in chunksOf maxX grid'
  