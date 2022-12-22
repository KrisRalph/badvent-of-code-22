module DayTwelve where

import Data.Char
import Data.HashSet (HashSet)
import Data.HashSet qualified as HashSet
import Data.List (find, foldl')
import Data.Maybe (fromMaybe, mapMaybe, maybeToList)
import Data.PQueue.Prio.Min qualified as MinPQueue
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.IO qualified as Text.IO
import Text.Parsec
import Text.Parsec.Text
import Utils (tshow)
import Utils.Grid2d

data Hill = Hill Int | Start | End
  deriving stock (Eq)

elevation :: Hill -> Int
elevation (Hill i) = i
elevation Start = letterToElevation 'a'
elevation End = letterToElevation 'z'

instance Show Hill where
  show (Hill i) = [chr (i + 96)]
  show Start = "S"
  show End = "E"

displayHeightmap :: Grid Index Hill -> Text
displayHeightmap = Text.stripEnd . Text.unlines . fmap formatLine . gridTo2dList
  where
    formatLine hs = Text.concat $ tshow <$> hs

-- time to parse
letters :: [Char]
letters = ['a' .. 'z']

letterToElevation :: Char -> Int
letterToElevation c = ord c - 96

heightBetweenPoints :: Hill -> Hill -> Int
heightBetweenPoints this next = elevation next - elevation this

satisfiesHeightRestriction :: Hill -> Hill -> Bool
satisfiesHeightRestriction this next = heightBetweenPoints this next <= 1

parseHill :: Parser Hill
parseHill =
  try (Hill . letterToElevation <$> oneOf letters)
    <|> try (Start <$ char 'S')
    <|> End <$ char 'E'

parseRange :: Parser (Grid Index Hill)
parseRange = gridFrom2dList <$> many1 parseHill `sepEndBy` newline <* eof

data GridZipper a = GridZipper
  { startIndex :: Index,
    endIndex :: Index,
    currentIndex :: Index,
    stepsTaken :: Int
  }
  deriving stock (Eq)

instance (Show a) => Show (GridZipper a) where
  show (GridZipper _ _ _ steps) =
    "GridZipper "
      <> "steps = "
      <> show steps
      <> "}"

mkHeightmapZipper :: Grid Index Hill -> Maybe (GridZipper Hill)
mkHeightmapZipper g = do
  startIdx <- gridIndexOf g Start
  endIdx <- gridIndexOf g End
  pure $ GridZipper startIdx endIdx startIdx 0

mkMultipleZippersForStarts :: Grid Index Hill -> [GridZipper Hill]
mkMultipleZippersForStarts g =
  -- change all a's to s's
  let lowAreas = gridAllIndexesOf g (Hill (letterToElevation 'a'))
      originalStart = maybeToList $ gridIndexOf g Start
      starts = originalStart <> lowAreas
      end = gridIndexOf g End
   in case end of
        Just endIdx -> [GridZipper startIdx endIdx startIdx 0 | startIdx <- starts]
        Nothing -> []

goSomewhere :: (Index -> Index) -> (a -> a -> Bool) -> Grid Index a -> GridZipper a -> Maybe (GridZipper a)
goSomewhere indexMove restriction g (GridZipper startIdx endIdx curIdx steps) = do
  let newIdx = indexMove curIdx
      newSteps = steps + 1
  val <- g `gridAt` curIdx
  newVal <- g `gridAt` newIdx -- check this index exists
  if restriction val newVal && gridIndexIsInBounds g newIdx
    then pure $ GridZipper startIdx endIdx newIdx newSteps
    else Nothing

indexLeft, indexRight, indexUp, indexDown :: Index -> Index
indexLeft (Index y x) = Index y (x - 1)
indexRight (Index y x) = Index y (x + 1)
indexUp (Index y x) = Index (y - 1) x
indexDown (Index y x) = Index (y + 1) x

isAtEnd :: GridZipper Hill -> Bool
isAtEnd gz = currentIndex gz == endIndex gz

currentHeight :: Grid Index Hill -> GridZipper Hill -> Int
currentHeight g gz = maybe (-999) elevation (g `gridAt` currentIndex gz)

incline :: Grid Index Hill -> GridZipper Hill -> GridZipper Hill -> Int
incline g this next = currentHeight g next - currentHeight g this

getNeighbours :: Grid Index Hill -> GridZipper Hill -> HashSet Index -> [GridZipper Hill]
getNeighbours g gz seen = do
  let curIdx = currentIndex gz
      isNewCoord indexer = not $ HashSet.member (indexer curIdx) seen
      possibleNeighbours = filter isNewCoord [indexLeft, indexRight, indexDown, indexUp]
      goToNeighbour grid zipper indexer = goSomewhere indexer satisfiesHeightRestriction grid zipper
  mapMaybe (goToNeighbour g gz) possibleNeighbours

bfs :: Grid Index Hill -> GridZipper Hill -> Maybe Int
bfs g gz = stepsTaken <$> loop [gz]
  where
    loop gzs
      | any isAtEnd gzs = find isAtEnd gzs
      | otherwise = loop $ concatMap (\z -> getNeighbours g z HashSet.empty) gzs

astar :: Grid Index Hill -> GridZipper Hill -> Maybe Int
astar g start = stepsTaken <$> loop pqueue HashSet.empty
  where
    pqueue = MinPQueue.singleton (cost start) start
    loop next seen = do
      gz' <- snd <$> MinPQueue.getMin next
      let newGzs = getNeighbours g gz' seen
          seen' = foldl' (flip HashSet.insert) seen (currentIndex <$> newGzs)
          doInsert prios x = MinPQueue.insertBehind (cost x) x prios
          nextItems = (\prios -> foldl' doInsert prios newGzs) (MinPQueue.deleteMin next)
      if any isAtEnd newGzs
        then find isAtEnd newGzs
        else loop nextItems seen'

cost :: GridZipper Hill -> Int
cost gz =
  let (Index endY endX) = endIndex gz
      (Index curY curX) = currentIndex gz
   in abs (endX - curX) + abs (endY - curY)

hillsText :: IO Text
hillsText = Text.IO.readFile "./files/DayTwelve/DayTwelve.txt"

main :: IO ()
main = do
  hillsContents <- hillsText
  let parseResults = parse parseRange "" hillsContents
  case parseResults of
    Left err -> putStrLn ("parse err: " <> show err)
    Right hills -> do
      let gz = fromMaybe (error "heightmap seems off") $ mkHeightmapZipper hills
          astarRes = maybe "no paths found" show $ astar hills gz
      putStrLn $ "QA: " <> astarRes

      let allStarts = mkMultipleZippersForStarts hills
          allTotals = mapMaybe (astar hills) allStarts
      putStrLn $ "QB: " <> show (minimum allTotals)
