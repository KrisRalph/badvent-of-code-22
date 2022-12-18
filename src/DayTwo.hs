module DayTwo (main) where

import Data.Text (Text)
import Data.Text.IO qualified as Text.IO
import Text.Parsec
import Text.Parsec.Text

data RPS = Rock | Paper | Scissors
  deriving stock (Show)

data Outcome = Win | Loss | Draw
  deriving stock (Show)

data Round = Round {theirs :: RPS, yours :: RPS, outcome :: Outcome, score :: Int}
  deriving stock (Show)

parseRPS :: Parser RPS
parseRPS =
  Rock <$ oneOf "AX"
    <|> Paper <$ oneOf "BY"
    <|> Scissors <$ oneOf "CZ"

parseOutcome :: Parser Outcome
parseOutcome =
  Loss <$ string "X"
    <|> Draw <$ string "Y"
    <|> Win <$ string "Z"

parseRound :: (RPS -> Parser RPS) -> Parser Round
parseRound parseYours = do
  theirs <- parseRPS <?> "Their RPS value"
  _ <- skipMany1 space <?> "A space"
  yours <- parseYours theirs
  _ <- skipMany endOfLine <?> "read to next line"
  let outcome = pickWinner theirs yours
  let score = scoreRound yours outcome
  pure $ Round {..}

parseRoundA :: Parser Round
parseRoundA = parseRound (\_ -> parseRPS <?> "Your RPS value")

parseRoundB :: Parser Round
parseRoundB = parseRound (\theirs -> pickResultFor theirs <$> parseOutcome <?> "Your desired outcome")

parseStrategyFileA :: Parser [Round]
parseStrategyFileA = manyTill parseRoundA eof

parseStrategyFileB :: Parser [Round]
parseStrategyFileB = manyTill parseRoundB eof

pickWinner :: RPS -> RPS -> Outcome
pickWinner yours theirs = case (yours, theirs) of
  (Rock, Scissors) -> Loss
  (Rock, Paper) -> Win
  (Paper, Rock) -> Loss
  (Paper, Scissors) -> Win
  (Scissors, Rock) -> Win
  (Scissors, Paper) -> Loss
  (Rock, Rock) -> Draw
  (Paper, Paper) -> Draw
  (Scissors, Scissors) -> Draw

pickResultFor :: RPS -> Outcome -> RPS
pickResultFor theirs desired = case (theirs, desired) of
  (Rock, Win) -> Paper
  (Rock, Draw) -> Rock
  (Rock, Loss) -> Scissors
  (Paper, Win) -> Scissors
  (Paper, Draw) -> Paper
  (Paper, Loss) -> Rock
  (Scissors, Win) -> Rock
  (Scissors, Draw) -> Scissors
  (Scissors, Loss) -> Paper

scoreRound :: RPS -> Outcome -> Int
scoreRound yours outcome = rpsScore + outcomeScore
  where
    outcomeScore = case outcome of
      Loss -> 0
      Draw -> 3
      Win -> 6
    rpsScore = case yours of
      Rock -> 1
      Paper -> 2
      Scissors -> 3

rockPaperScissorsList :: IO Text
rockPaperScissorsList = Text.IO.readFile "./files/DayTwo/DayTwo.txt"

runAParser :: Parser [Round] -> Text -> IO ()
runAParser parser inputText = do
  let parsedStrat = parse parser "" inputText
  case parsedStrat of
    Left err -> putStrLn ("parseError: " <> show err)
    Right rounds -> print $ foldr ((+) . score) 0 rounds

main :: IO ()
main = do
  inputList <- rockPaperScissorsList
  putStr "QA: " >> runAParser parseStrategyFileA inputList
  putStr "QB: " >> runAParser parseStrategyFileB inputList
