module DayFive (main) where

import Control.Exception (Exception)
import Control.Monad (foldM)
import Data.Data (Typeable)
import Data.Functor (void)
import Data.IntMap (IntMap)
import Data.IntMap qualified as IntMap
import Data.List (transpose)
import Data.Maybe (catMaybes)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.IO qualified as Text.IO
import Text.Parsec
import Text.Parsec.Text
import Utils (number)

newtype Crate = Crate {crateLabel :: Text}
  deriving stock (Show, Eq)

parseCrate :: Parser Crate
parseCrate = Crate <$ string "[" <*> (Text.singleton <$> alphaNum) <* string "]"

-- a hole in the grid is exactly three spaces, they're separated by a space.
parseGap :: Parser ()
parseGap = void $ string (replicate 3 ' ')

data Move = Move {quantity :: Int, fromCol :: Int, toCol :: Int}
  deriving stock (Show)

skipString :: String -> Parser ()
skipString s = string s >> spaces

number' :: Parser Int
number' = number <* spaces

parseMove :: Parser Move
parseMove = do
  skipString "move"
  quantity <- number'
  skipString "from"
  fromCol <- number'
  skipString "to"
  toCol <- number'
  return $ Move {..}

newtype Stack a = Stack [a]
  deriving stock (Show)

emptyStack :: Stack a
emptyStack = Stack []

stackFromList :: [a] -> Stack a
stackFromList [] = emptyStack
stackFromList xs = Stack xs

stackToList :: Stack a -> [a]
stackToList (Stack xs) = xs

push :: a -> Stack a -> Stack a
push x (Stack xs) = Stack (x : xs)

pushMultiple :: [a] -> Stack a -> Stack a
pushMultiple xs (Stack ys) = Stack (xs <> ys)

pop :: Stack a -> (Maybe a, Stack a)
pop (Stack []) = (Nothing, Stack [])
pop (Stack (x : xs)) = (Just x, Stack xs)

popMultiple :: Int -> Stack a -> (Maybe [a], Stack a)
popMultiple _ (Stack []) = (Nothing, Stack [])
popMultiple n (Stack xs) =
  if length xs >= n
    then (Just (take n xs), Stack (drop n xs))
    else (Nothing, Stack xs)

data CrateStacks = CrateStacks {width :: Int, crates :: IntMap (Stack Crate), moves :: [Move]}
  deriving stock (Show)

parseGrid :: Parser [Stack Crate]
parseGrid = do
  parseRows <- manyTill parseLine endMarker
  pure . fmap (stackFromList . catMaybes) . transpose $ parseRows
  where
    parseLine = parseCrateRow <* newline
    endMarker = try (space >> lookAhead digit)

-- parses "    [D]    \n" into [Nothing, Just (Crate "D"), Nothing]
parseCrateRow :: Parser [Maybe Crate]
parseCrateRow = sepEndBy1 parseRowEntry (string " ")

parseRowEntry :: Parser (Maybe Crate)
parseRowEntry =
  Just <$> (parseCrate <?> "a crate like '[A]'")
    <|> (Nothing <$ parseGap <?> "explicitly 3 spaces")

parseNumberLine :: Parser [Int]
parseNumberLine = sepEndBy1 number spaces

parseMoves :: Parser [Move]
parseMoves = many1 parseMove

parseCrateStacks :: Parser CrateStacks
parseCrateStacks = do
  crateRows <- parseGrid
  numberLine <- parseNumberLine
  moves <- parseMoves
  let crates = makeKeyedList crateRows
  let numberLineFitsRows = length crates == last numberLine
  let width = last numberLine
  if not numberLineFitsRows
    then fail "all stacks must be numbered"
    else return CrateStacks {..}

makeKeyedList :: [Stack a] -> IntMap (Stack a)
makeKeyedList xss = IntMap.fromList $ go 1 xss
  where
    go :: Int -> [Stack a] -> [(Int, Stack a)]
    go _ [] = []
    go n (xs : xss') = (n, xs) : go (n + 1) xss'

displayTopOfStack :: CrateStacks -> Text
displayTopOfStack (CrateStacks _ crates _) =
  let displayedStacks = stackToList . snd <$> IntMap.toList crates
      transposedStacks = transpose displayedStacks
   in Text.concat $ crateLabel <$> head transposedStacks

crateListText :: IO Text
crateListText = Text.IO.readFile "./files/DayFive/DayFiveTest.txt"

data MoveType = Single | Multiple
  deriving stock (Show)

doMoves :: MoveType -> [Move] -> IntMap (Stack Crate) -> Maybe (IntMap (Stack Crate))
doMoves typ ms imap = foldM (doMovelet typ) imap ms

doMovelet :: MoveType -> IntMap (Stack Crate) -> Move -> Maybe (IntMap (Stack Crate))
doMovelet typ cols (Move quantity fromCol toCol) = do
  fromColumn <- IntMap.lookup fromCol cols
  toColumn <- IntMap.lookup toCol cols
  (fromCol', toCol') <- moveFn quantity fromColumn toColumn
  return . IntMap.insert fromCol fromCol' . IntMap.insert toCol toCol' $ cols
  where
    moveFn = case typ of
      Single -> doPushPops
      Multiple -> doPickup

doPushPops :: Int -> Stack a -> Stack a -> Maybe (Stack a, Stack a)
doPushPops 0 l r = Just (l, r)
doPushPops n l r = do
  (l', r') <- pushPop l r
  doPushPops (n - 1) l' r'

pushPop :: Stack a -> Stack a -> Maybe (Stack a, Stack a)
pushPop l r = do
  let (mlVal, l') = pop l
  lVal <- mlVal
  return (l', push lVal r)

doPickup :: Int -> Stack a -> Stack a -> Maybe (Stack a, Stack a)
doPickup 0 l r = Just (l, r)
doPickup n l r = do
  let (mLs, l') = popMultiple n l
  ls <- mLs
  return (l', pushMultiple ls r)

newtype InvalidCratesError = InvalidCratesError String
  deriving stock (Typeable, Show)

instance Exception InvalidCratesError

processCrateStacks :: MoveType -> CrateStacks -> Either InvalidCratesError CrateStacks
processCrateStacks typ (CrateStacks width crates moves) = do
  let doneMoves = doMoves typ moves crates
  case doneMoves of
    Nothing -> Left (InvalidCratesError "moves were unsuccessful")
    Just crates' -> Right $ CrateStacks {width = width, crates = crates', moves = []}

main :: IO ()
main = do
  crateList <- crateListText
  let parseResults = parse parseCrateStacks "" crateList
  case parseResults of
    Left err -> print err
    Right result ->
      let resultA = either (error . show) id (processCrateStacks Single result)
          resultB = either (error . show) id (processCrateStacks Multiple result)
       in do
            Text.IO.putStrLn $ "QA: " <> displayTopOfStack resultA
            Text.IO.putStrLn $ "QB: " <> displayTopOfStack resultB
