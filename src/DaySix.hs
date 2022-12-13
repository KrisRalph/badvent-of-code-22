module DaySix (main) where

import Data.HashSet (HashSet)
import Data.HashSet qualified as HashSet
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.IO qualified as Text.IO
import Utils (tshow)

isStartMarker :: Int -> Text -> Bool
isStartMarker n cs = Text.length cs == n && go cs HashSet.empty
  where
    go :: Text -> HashSet Char -> Bool
    go txt _ | txt == Text.empty = True
    go txt seen = fromMaybe False $ do
      (t, ts) <- Text.uncons txt
      pure $ not (t `HashSet.member` seen) && go ts (HashSet.insert t seen)

-- i'm using this zipper to slide across the block of text
data Zipper a = Zipper {width :: Int, index :: Int, prev :: [a], current :: a, rest :: a}
  deriving stock (Show)

slidingWindow :: Int -> Text -> Zipper Text
slidingWindow width = Zipper width 0 [] ""

right :: Zipper Text -> Zipper Text
right (Zipper width i prevs current rest) = Zipper width (i + 1) (current : prevs) current' rest'
  where
    rest' = Text.drop 1 rest
    current' = Text.take width rest'

rightWhile :: (Text -> Bool) -> Zipper Text -> Zipper Text
rightWhile cond z = if cond (current z) then rightWhile cond (right z) else z

findFirstMarker :: Int -> Text -> Int
findFirstMarker offset t =
  (+ offset) . index $
    rightWhile (not . isStartMarker offset) (slidingWindow offset t)

findFirstStartMarker :: Text -> Int
findFirstStartMarker = findFirstMarker 4

findFirstMessageMarker :: Text -> Int
findFirstMessageMarker = findFirstMarker 14

signalList :: IO Text
signalList = Text.IO.readFile "./files/DaySix/DaySix.txt"

main :: IO ()
main = do
  signal <- signalList
  let markerIdx = findFirstStartMarker signal
  Text.IO.putStrLn $ "QA: " <> tshow markerIdx
  let messageIdx = findFirstMessageMarker signal
  Text.IO.putStrLn $ "QB: " <> tshow messageIdx
