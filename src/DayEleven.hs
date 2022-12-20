{-# LANGUAGE NumericUnderscores #-}

module DayEleven (main) where

import Data.Function (on)
import Data.IntMap (IntMap)
import Data.IntMap.Strict qualified as IntMap
import Data.List (sortBy)
import Data.Text (Text)
import Data.Text.IO qualified as Text.IO
import Text.Parsec
import Text.Parsec.Text
import Utils (number, string')
import Data.Maybe (fromMaybe)

data Value = Value Int | Old
  deriving stock (Show)

data Operation = Multiply Value | Divide Value | Plus Value | Minus Value
  deriving stock (Show)

newtype Test = Divisible { by :: Int }
  deriving stock (Show)

data Consequences = Consequences {ifTrue :: Int, ifFalse :: Int}
  deriving stock (Show)

data Monkey = Monkey {idNumber :: Int, items :: [Int], operation :: Operation, test :: Test, consequences :: Consequences, timesInspected :: Int}
  deriving stock (Show)

comma :: Parser String
comma = string' ","

value :: Parser Value
value = Old <$ try (string' "old") <|> Value <$> number

monkeyOpFromString :: Parser Operation
monkeyOpFromString = try mul <|> try division <|> try plus <|> minus
  where
    mul = Multiply <$ string' "*" <*> value
    division = Divide <$ string' "/" <*> value
    plus = Plus <$ string' "+" <*> value
    minus = Minus <$ string' "-" <*> value

parseOperation :: Parser Operation
parseOperation = string' "Operation: new = old" *> monkeyOpFromString <* spaces

parseConds :: Parser Consequences
parseConds = do
  ifTrue <- string' "If true: throw to monkey" *> number <* spaces
  ifFalse <- string' "If false: throw to monkey" *> number <* spaces
  return $ Consequences {..}

parseMonkeyTest :: Parser (Test, Consequences)
parseMonkeyTest = do
  operator <- string' "Test:" *> parseOp
  test <- operator <$> number <* spaces
  consequences <- parseConds
  return (test, consequences)
  where
    parseOp = Divisible <$ try (string' "divisible by")

parseMonkey :: Parser Monkey
parseMonkey = do
  idNumber <- string' "Monkey" *> number <* string' ":"
  items <- string' "Starting items:" *> number `sepBy` comma <* spaces
  operation <- parseOperation
  let timesInspected = 0
  (test, consequences) <- parseMonkeyTest
  return $ Monkey {..}

parseMonkeyFile :: Parser (IntMap Monkey)
parseMonkeyFile = do
  monkeys <- many1 parseMonkey
  return $ IntMap.fromList [(idNumber monkey, monkey) | monkey <- monkeys]

doOp :: Operation -> Int -> Int
doOp op i = case op of
  Multiply v -> newWorry (*) v i
  Divide v -> newWorry div v i
  Plus v -> newWorry (+) v i
  Minus v -> newWorry (-) v i
  where
    newWorry func v worry = case v of
      Value v' -> worry `func` v'
      Old -> worry `func` worry

relief :: Int -> Int
relief i = i `div` 3

inspectItems :: Bool -> Monkey -> Monkey
inspectItems getRelieved m =
  let worryingItems = doOp (operation m) <$> items m 
      possiblyLessenWorry = if getRelieved then fmap relief else id
   in m {items = possiblyLessenWorry worryingItems, timesInspected = timesInspected m + length worryingItems}

throwItems :: Monkey -> [Int] -> IntMap Monkey -> IntMap Monkey
throwItems = go
  where
    go :: Monkey -> [Int] -> IntMap Monkey -> IntMap Monkey
    go _ [] others' = others'
    go from' (t : ts) others' =
      case t `IntMap.lookup` others' of
        Nothing -> error "couldn't find target"
        Just to -> do
          let (lighterMonkey, addressee) = doThrow from' to
              others'' =
                IntMap.insert (idNumber lighterMonkey) lighterMonkey
                  . IntMap.insert (idNumber addressee) addressee
                  $ others'
           in go lighterMonkey ts others''

    doThrow :: Monkey -> Monkey -> (Monkey, Monkey)
    doThrow from' to =
      let item = head (items from')
          toItems = items to <> [item]
          fromItems = drop 1 (items from')
          (fromThrown, toThrown) = (from' {items = fromItems}, to {items = toItems})
       in (fromThrown, toThrown)

testItem :: Int -> Test -> Bool
testItem i test =
  case test of
    Divisible d -> i `rem` d == 0

findTarget :: Consequences -> Bool -> Int
findTarget cs b = if b then ifTrue cs else ifFalse cs

takeTurn :: Bool -> Int -> IntMap Monkey -> IntMap Monkey
takeTurn getRelieved mId others =
  let m = fromMaybe (error "monkey not found") $ mId `IntMap.lookup` others
      monkey = inspectItems getRelieved m
      is = items monkey
      tests = (`testItem` test monkey) <$> is
      targets = findTarget (consequences monkey) <$> tests
   in throwItems monkey targets others

destress :: IntMap Monkey -> IntMap Monkey
destress ms = 
  let allDivisors = IntMap.map (by . test) ms
      lcmValue = foldr lcm 1 allDivisors
      unWorry m = 
        let items' = (`mod` lcmValue) <$> items m 
        in m { items = items' }
  in IntMap.map unWorry ms

doRound :: Bool -> IntMap Monkey -> IntMap Monkey
doRound getRelieved monkeys = go (IntMap.keys monkeys) monkeys
  where
    -- sigh i need the first element and the imap again aa
    go [] imap = imap
    go (mId : ms) imap = 
      let imap' = takeTurn getRelieved mId imap 
          relaxedImap = if not getRelieved then destress imap' else imap'
      in go ms relaxedImap

nTimes :: Int -> (a -> a) -> a -> a
nTimes 0 _ im = im
nTimes n f im = nTimes (n - 1) f (f im)

doTwentyRounds :: IntMap Monkey -> IntMap Monkey
doTwentyRounds = nTimes 20 (doRound True)

doTenThousandRounds :: IntMap Monkey -> IntMap Monkey
doTenThousandRounds = nTimes 10_000 (doRound False)

calculateMonkeyBusiness :: IntMap Monkey -> (Int, Int)
calculateMonkeyBusiness monkeys =
  let monkeys' = doTwentyRounds monkeys
      oldMonkeys = doTenThousandRounds monkeys
  in (toRes monkeys', toRes oldMonkeys)
  where 
    toRes imap =
      let toList = sortBy (flip compare `on` timesInspected) $ snd <$> IntMap.toList imap
          topTwo = take 2 toList
      in product (timesInspected <$> topTwo)

monkeysFile :: IO Text
monkeysFile = Text.IO.readFile "./files/DayEleven/DayEleven.txt"

main :: IO ()
main = do
  monkeyText <- monkeysFile
  let parseResults = parse parseMonkeyFile "" monkeyText
  case parseResults of
    Left err -> putStrLn ("parse err: " <> show err)
    Right monkeyMap -> do
      let (qaRes, qbRes) = calculateMonkeyBusiness monkeyMap
      putStrLn ("QA: " <> show qaRes)
      putStrLn ("QB: " <> show qbRes)
