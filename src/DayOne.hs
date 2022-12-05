{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module DayOne (main) where

import Control.Monad (join)
import Data.Function (on)
import Data.List (maximumBy)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.IO qualified as Text.IO
import Text.Read

elfList :: IO Text
elfList = Text.IO.readFile "./files/DayOneElfList.txt"

dayOneQ :: Text -> [Int]
dayOneQ t = maximumBy (compare `on` sum) readInts
  where
    splitLines = filter ((/= 0) . Text.length) . Text.splitOn "\n" <$> Text.splitOn "\n\n" t
    readableStrings = fmap Text.unpack <$> splitLines
    readInts = fmap (read @Int) <$> readableStrings

main :: IO ()
main = elfList >>= print . dayOneQ
