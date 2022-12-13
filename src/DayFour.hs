module DayFour where

import Data.Text (Text)
import Data.Text.IO qualified as Text.IO
import Text.Parsec
import Text.Parsec.Text
import Utils (number)

campCleanupList :: IO Text
campCleanupList = Text.IO.readFile "./files/DayFour/DayFour.txt"

data Assignment = Assignment {lowerBound :: Int, upperBound :: Int}
  deriving stock (Show)

data Pairing = Pairing {leftElf :: Assignment, rightElf :: Assignment, overlapping :: Bool, overlapsAtAll :: Bool}
  deriving stock (Show)

contains :: Assignment -> Assignment -> Bool
contains a b = lowerBound a <= lowerBound b && upperBound a >= upperBound b

checkContains :: Assignment -> Assignment -> Bool
checkContains a b = contains a b || contains b a

-- this is probably not the most efficient way to implement this, and i'm okay with that.
checkOverlaps :: Assignment -> Assignment -> Bool
checkOverlaps a b = lowerBound b `elem` [lowerBound a .. upperBound a]

checkOverlapsAtAll :: Assignment -> Assignment -> Bool
checkOverlapsAtAll a b = checkOverlaps a b || checkOverlaps b a

parseAssignment :: Parser Assignment
parseAssignment = Assignment <$> number <* string "-" <*> number

parsePairing :: Parser Pairing
parsePairing = do
  leftElf <- parseAssignment
  _ <- string ","
  rightElf <- parseAssignment
  _ <- skipMany endOfLine
  let overlapping = checkContains leftElf rightElf
  let overlapsAtAll = checkOverlapsAtAll leftElf rightElf
  return $ Pairing {..}

parseAssignmentFile :: Parser [Pairing]
parseAssignmentFile = manyTill parsePairing eof

main :: IO ()
main = do
  campCleanup <- campCleanupList
  let parseResults = parse parseAssignmentFile "" campCleanup
  case parseResults of
    Left err -> putStrLn ("parse err: " <> show err)
    Right assignments ->
      let overlaps = length (filter overlapping assignments)
          maximumOverlapping = length (filter overlapsAtAll assignments)
       in do
            putStrLn ("QA: " <> show overlaps)
            putStrLn ("QB: " <> show maximumOverlapping)
