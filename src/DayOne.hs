module DayOne (main) where

import Data.Function (on)
import Data.List (maximumBy, sortBy)
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.IO qualified as Text.IO
import Text.Read (readMaybe)

elfList :: IO Text
elfList = Text.IO.readFile "./files/DayOne/DayOne.txt"

dayOneQA :: Text -> [Int]
dayOneQA t = maximumBy (compare `on` sum) $ readInts t

dayOneQB :: Text -> [[Int]]
dayOneQB t = take 3 . sortBy (flip compare `on` sum) $ readInts t

readInts :: Text -> [[Int]]
readInts t = mapMaybe readMaybe <$> readableStrings
  where
    splitLines = filter ((/= 0) . Text.length) . Text.splitOn "\n" <$> Text.splitOn "\n\n" t
    readableStrings = fmap Text.unpack <$> splitLines

main :: IO ()
main = do
  elves <- elfList
  let dayOneQARes = dayOneQA elves
  putStrLn ("QA: " ++ show dayOneQARes ++ " " ++ show (sum dayOneQARes))

  let dayOneQBRes = dayOneQB elves
  putStrLn ("QB: " ++ show dayOneQBRes ++ " " ++ show (sum . fmap sum $ dayOneQBRes))
