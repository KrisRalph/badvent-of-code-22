{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module DayThree (main) where

import Data.HashSet qualified as HashSet
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.IO qualified as Text.IO

data Backpack = Backpack {left :: Text, right :: Text}
  deriving stock (Show, Eq, Ord)

backpackList :: IO Text
backpackList = Text.IO.readFile "./files/DayThree/DayThree.txt"

priorities :: Map Char Int
priorities = Map.fromList (smallLetterMap <> largeLetterMap)
  where
    smallLetterMap = zip ['a' .. 'z'] [1 .. 27]
    largeLetterMap = zip ['A' .. 'Z'] [27 .. 52]

parseBackpack :: Text -> Backpack
parseBackpack t = Backpack {..}
  where
    (left, right) = Text.splitAt (Text.length t `div` 2) t

getIntersection :: Text -> Text -> Text
getIntersection l r =
  let lSet = HashSet.fromList (Text.unpack l)
      rSet = HashSet.fromList (Text.unpack r)
   in Text.pack . HashSet.toList $ HashSet.intersection lSet rSet

priority :: Backpack -> Int
priority (Backpack l r) =
  let is = getIntersection l r
      scoreIntersectedItem c = fromMaybe 0 $ Map.lookup c priorities
   in Text.foldr ((+) . scoreIntersectedItem) 0 is

intersect3 :: Backpack -> Backpack -> Backpack -> Text
intersect3 b c d =
  let toSet bp = HashSet.fromList . Text.unpack $ left bp <> right bp
      hashSets = toSet <$> [b, c, d]
   in Text.pack . HashSet.toList $
        foldr1 HashSet.intersection hashSets

chunksOf :: Int -> [a] -> [[a]]
chunksOf 1 s = [s]
chunksOf n xs = fromMaybe [] $ do
  (chunk, remainder) <- splitAtMay n xs
  return $ chunk : chunksOf n remainder

splitAtMay :: Int -> [a] -> Maybe ([a], [a])
splitAtMay n xs = if length xs >= n then Just (splitAt n xs) else Nothing

tuplify3 :: Ord a => [a] -> [(a, a, a)]
tuplify3 xs = mapMaybe toTup (chunksOf 3 xs)
  where
    toTup [x, y, z] = Just (x, y, z)
    toTup _ = Nothing

data ElfGroup = ElfGroup {elves :: (Backpack, Backpack, Backpack), needle :: Maybe Char, groupPriority :: Int}
  deriving stock (Show)

mkElfGroup :: (Backpack, Backpack, Backpack) -> ElfGroup
mkElfGroup (b, c, d) = ElfGroup {elves = (b, c, d), needle = commonItem, groupPriority = getGroupPrio commonItem}
  where
    commonItem = fst <$> Text.uncons (intersect3 b c d)
    getGroupPrio needle =
      fromMaybe 0 $
        needle >>= \n -> return . sum $ Map.lookup n priorities

findGroups :: [Backpack] -> [ElfGroup]
findGroups bs = mkElfGroup <$> tuplify3 bs

main :: IO ()
main = do
  inputText <- backpackList
  let backpacks = parseBackpack <$> Text.splitOn "\n" inputText

  let totalPriority = foldr ((+) . priority) 0 backpacks
  putStrLn $ "QA: " <> show totalPriority

  let groups = findGroups backpacks
  let totalPriorityForGroups = foldr ((+) . groupPriority) 0 groups
  putStrLn $ "QB: " <> show totalPriorityForGroups
