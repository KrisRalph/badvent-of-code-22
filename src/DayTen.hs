module DayTen (main) where

import Data.List (find, scanl', sortOn)
import Data.Maybe (isJust, mapMaybe)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.IO qualified as Text.IO
import Text.Parsec
import Text.Parsec.Text
import Utils (number, string')
import Utils.Grid2d

data Op = Noop | Addx Int
  deriving stock (Show)

data CPU = CPU {x :: Int, cycles :: Int, crt :: CRT}
  deriving stock (Show)

data CRT = CRT {curX :: Int, curY :: Int, pixels :: Grid Index Bool}
  deriving stock (Show)

newCRT :: CRT
newCRT = CRT 0 0 (gridFrom2dList (replicate 6 (replicate 40 False)))

newCPU :: CPU
newCPU = CPU 1 1 newCRT

parseOp :: Parser Op
parseOp =
  try (Noop <$ string' "noop")
    <|> Addx <$ string' "addx" <*> number

cyclesPerOp :: Op -> Int
cyclesPerOp = \case
  Addx _ -> 2
  Noop -> 1

signalStrength :: CPU -> Int
signalStrength (CPU x cycles _) = cycles * x

getClosestCycle :: Int -> [CPU] -> Maybe CPU
getClosestCycle n stateList =
  let idealMatch = find ((== n) . cycles) stateList
   in if isJust idealMatch then idealMatch else tryFindBestMatch n stateList
  where
    tryFindBestMatch n' states =
      let diffsToTarget = map (\cpu -> (n' - cycles cpu, cpu)) states
          sortedDiffs = sortOn fst diffsToTarget
          probablyBestOne = find ((>= 0) . fst) sortedDiffs
          fixupCycles cpu = cpu {cycles = n'} -- bodge cycle to happen during instruction
       in fixupCycles . snd <$> probablyBestOne

runOp :: CPU -> Op -> CPU
runOp cpu op = case op of
  Addx i -> cpu {x = x cpu + i, cycles = cycles', crt = crt''}
  Noop -> cpu {cycles = cycles', crt = crt'}
  where
    cycles' = cycles cpu + cyclesPerOp op
    crt' = runDrawCRT (x cpu) (crt cpu)
    crt'' = runDrawCRT (x cpu) . runDrawCRT (x cpu) $ crt cpu

runDrawCRT :: Int -> CRT -> CRT
runDrawCRT hpos (CRT curX curY pixels) =
  let curX' = if curX == 39 then 0 else curX + 1
      curY' = if curX == 39 then curY + 1 else curY
      pixelPositions = [hpos - 1, hpos, hpos + 1]
      overlapsHpos = (curX `elem` pixelPositions)
      pixels' = gridSetAt pixels (Index curY curX) True
   in if overlapsHpos then CRT curX' curY' pixels' else CRT curX' curY' pixels

getTargetCycles :: [CPU] -> [CPU]
getTargetCycles states =
  let lastCycle = cycles $ last states
      targetCycles = [20, 60 .. lastCycle]
   in mapMaybe (`getClosestCycle` states) targetCycles

runOps :: CPU -> [Op] -> [CPU]
runOps = scanl' runOp

getSignalStrength :: [CPU] -> Int
getSignalStrength = sum . fmap signalStrength

displayCRT :: CRT -> Text
displayCRT (CRT _ _ pixels) = displayBools (gridTo2dList pixels)

displayBools :: [[Bool]] -> Text
displayBools = Text.stripEnd . Text.unlines . fmap formatBoolLine
  where
    formatBoolLine bs = Text.concat $ formatBool <$> bs
    formatBool True = "#"
    formatBool False = "."

parseInstructionsFile :: Parser [Op]
parseInstructionsFile = manyTill (parseOp <* optional newline) eof

instructionsText :: IO Text
instructionsText = Text.IO.readFile "./files/DayTen/DayTen.txt"

main :: IO ()
main = do
  instructions <- instructionsText
  let parseResults = parse parseInstructionsFile "" instructions
  case parseResults of
    Left err -> putStrLn ("parse err: " <> show err)
    Right instrs -> do
      putStrLn $ "QA: " <> show (getSignalStrength . getTargetCycles . runOps newCPU $ instrs)
      Text.IO.putStrLn $ "QB:\n" <> displayCRT (crt . last $ runOps newCPU instrs)
