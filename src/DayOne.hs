{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module DayOne (main) where

import Control.Monad (join)
import Data.Function (on)
import Data.List (maximumBy)
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.IO qualified as Text.IO
import Text.Read (readMaybe)

elfList :: IO Text
elfList = Text.IO.readFile "./files/DayOneElfList.txt"

dayOneQA :: Text -> [Int]
dayOneQA t = maximumBy (compare `on` sum) readInts
  where
    splitLines = filter ((/= 0) . Text.length) . Text.splitOn "\n" <$> Text.splitOn "\n\n" t
    readableStrings = fmap Text.unpack <$> splitLines
    readInts = mapMaybe (readMaybe @Int) <$> readableStrings

main :: IO ()
main =
  elfList >>= print . dayOneQA
