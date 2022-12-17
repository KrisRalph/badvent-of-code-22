module DayNine (main) where

import Data.Containers.ListUtils (nubOrd)
import Data.List (foldl')
import Data.Text (Text)
import Data.Text.IO qualified as Text.IO
import Text.Parsec
import Text.Parsec.Text
import Utils (number, string')

data TailMove
  = MoveLeft
  | MoveRight
  | MoveUp
  | MoveDown
  | MoveUpLeft
  | MoveUpRight
  | MoveDownLeft
  | MoveDownRight
  deriving stock (Show)

data Direction
  = DirLeft
  | DirRight
  | DirUp
  | DirDown
  deriving stock (Show)

data MoveLine = MoveLine {direction :: Direction, numSpaces :: Int}
  deriving stock (Show)

parseLine :: Parser MoveLine
parseLine = MoveLine <$> parseDirection <*> number <* newline
  where
    parseDirection =
      try (DirLeft <$ string' "L")
        <|> try (DirRight <$ string' "R")
        <|> try (DirUp <$ string' "U")
        <|> try (DirDown <$ string' "D")

parseInputFile :: Parser [MoveLine]
parseInputFile = manyTill parseLine eof

tailMoveDirection :: Point -> TailMove -> Point
tailMoveDirection (Point x y) direction = case direction of
  MoveLeft -> Point (x - 1) y
  MoveRight -> Point (x + 1) y
  MoveUp -> Point x (y + 1)
  MoveDown -> Point x (y - 1)
  MoveUpLeft -> Point (x - 1) (y + 1)
  MoveUpRight -> Point (x + 1) (y + 1)
  MoveDownLeft -> Point (x - 1) (y - 1)
  MoveDownRight -> Point (x + 1) (y - 1)

dumbDistanceBetween :: Point -> Point -> Int
dumbDistanceBetween (Point y x) (Point b a) = abs (y - b) + abs (x - a)

moveHeadPoint :: Direction -> Point -> Point
moveHeadPoint dir (Point px py) = case dir of
  DirLeft -> Point (px - 1) py
  DirRight -> Point (px + 1) py
  DirUp -> Point px (py + 1)
  DirDown -> Point px (py - 1)

moveTailPoint :: Point -> Point -> Point
moveTailPoint curPoint tailPoint =
  let (Point curX curY) = curPoint
      (Point tailX tailY) = tailPoint
      (diffX, diffY) = (curX - tailX, curY - tailY)
      shouldMove = abs diffX >= 2 || abs diffY >= 2
      shouldMoveDiagonally = dumbDistanceBetween curPoint tailPoint >= 2 && curX /= tailX && curY /= tailY
      potentialMoveDiag = case (diffX, diffY) of
        (1, 2) -> Just MoveUpRight
        (2, 1) -> Just MoveUpRight
        (2, 2) -> Just MoveUpRight
        (1, -2) -> Just MoveDownRight
        (2, -1) -> Just MoveDownRight
        (2, -2) -> Just MoveDownRight
        (-2, 1) -> Just MoveUpLeft
        (-1, 2) -> Just MoveUpLeft
        (-2, 2) -> Just MoveUpLeft
        (-2, -1) -> Just MoveDownLeft
        (-1, -2) -> Just MoveDownLeft
        (-2, -2) -> Just MoveDownLeft
        -- missing move cases
        _ -> Nothing
      potentialMove = case (diffX, diffY) of
        (-2, 0) -> Just MoveLeft
        (2, 0) -> Just MoveRight
        (0, -2) -> Just MoveDown
        (0, 2) -> Just MoveUp
        _ -> Nothing
      potentialMoveTail
        | shouldMoveDiagonally = potentialMoveDiag
        | shouldMove = potentialMove
        | otherwise = Nothing
   in maybe tailPoint (tailMoveDirection tailPoint) potentialMoveTail

data Point = Point Int Int
  deriving stock (Show, Eq, Ord)

emptyPoints :: [Point]
emptyPoints = [Point 0 0]

pathsFromMoveList :: [MoveLine] -> [Point]
pathsFromMoveList mls =
  let moveSeq = concatMap (\ml -> replicate (numSpaces ml) (direction ml)) mls
   in foldl' (flip moveHeadPath) emptyPoints moveSeq

moveHeadPath :: Direction -> [Point] -> [Point]
moveHeadPath dir paths =
  let phead = head paths
   in moveHeadPoint dir phead : paths

makeNewTail :: [Point] -> [Point]
makeNewTail = scanr1 moveTailPoint

getNthTail :: Int -> [Point] -> [Point]
getNthTail 0 ts = ts
getNthTail n ts = let ts' = makeNewTail ts in getNthTail (n - 1) ts'

getNthTailVisits :: Int -> [MoveLine] -> Int
getNthTailVisits n mls =
  let ps = pathsFromMoveList mls
   in length . nubOrd $ getNthTail n ps

getUniqueTailVisits :: [MoveLine] -> Int
getUniqueTailVisits = getNthTailVisits 1

getTenthTailVisits :: [MoveLine] -> Int
getTenthTailVisits = getNthTailVisits 9

movesText :: IO Text
movesText = Text.IO.readFile "./files/DayNine/DayNine.txt"

main :: IO ()
main = do
  moves <- movesText
  let parseResults = parse parseInputFile "" moves
  case parseResults of
    Left err -> putStrLn ("parse err: " <> show err)
    Right moveses -> do
      putStrLn ("QA: " <> show (getUniqueTailVisits moveses))
      putStrLn ("QB: " <> show (getTenthTailVisits moveses))
