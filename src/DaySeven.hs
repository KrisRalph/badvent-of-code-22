{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-} -- ;A;

module DaySeven (main) where

import Control.Lens
import Data.Either (lefts, rights)
import Data.Function (on)
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HashMap
import Data.List (minimumBy)
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.IO qualified as Text.IO
import Text.Parsec

unacceptableSizeBound :: Integer
unacceptableSizeBound = 100000

totalSpace :: Integer
totalSpace = 70000000

requiredForUpdate :: Integer
requiredForUpdate = 30000000

data File = File
  { _parentPath :: Text,
    _fileName :: Text,
    _fileExt :: Text,
    _fileSize :: Integer
  }
  deriving stock (Show)

data Directory = Directory
  { _dirName :: Text,
    _dirPath :: Text,
    _files :: [File],
    _subDirNames :: [Text],
    _resolvedSubDirs :: [Directory],
    _resolvedSize :: Integer
  }
  deriving stock (Show)

newtype ChangeDir = ChangeDir {_cdTarget :: Text}
  deriving stock (Show)

data Command = CD ChangeDir | LS Directory
  deriving stock (Show)

data DirParserState = DirParserState
  { _indentLevel :: Int,
    _currentDir :: [Text],
    _dirMap :: HashMap Text Directory
  }
  deriving stock (Show)

type DirParser a = Parsec Text DirParserState a

-- ok, so,, i caved and used lenses...
$(makeLenses ''File)
$(makeLenses ''Directory)
$(makeLenses ''ChangeDir)
$(makeLenses ''DirParserState)

dirParse :: DirParser a -> SourceName -> Text -> Either ParseError a
dirParse p = runP p startState

startState :: DirParserState
startState = DirParserState {_indentLevel = 0, _currentDir = [], _dirMap = HashMap.empty}

parseCommand :: DirParser Command
parseCommand = do
  _ <- string' "$"
  cmd <- try (CD <$> parseCD) <|> (LS <$> parseLS)
  state <- getState
  let newState = case cmd of
        CD (ChangeDir "..") -> state & currentDir %~ tail
        CD (ChangeDir dir) -> state & currentDir %~ (dir :)
        LS _ -> state
  putState newState
  return cmd
  where
    parseCD = string' "cd" *> (ChangeDir . Text.pack <$> manyTill anyChar endOfLine <?> "changeDir")
    parseLS = string' "ls" *> parseDirectory <?> "directory"

parseDirName :: DirParser Text
parseDirName = string' "dir" *> (Text.pack <$> (manyTill anyChar endOfLine <?> "dirName"))

parseFile :: DirParser File
parseFile = do
  state <- getState
  _fileSize <- read <$> manyTill digit space <?> "fileSize"
  _fileName <- Text.pack <$> manyTill anyChar endOfLine <?> "fileName"
  let _fileExt = fromMaybe "" $ getFileExt _fileName
      _parentPath = intercalateTail "/" (reverse $ state ^. currentDir)
  pure $ File {..}

getFileExt :: Text -> Maybe Text
getFileExt t =
  let split = Text.splitOn "." t
      isExtensionless = length split == 1
   in if isExtensionless then Nothing else Just $ last split

intercalateTail :: Text -> [Text] -> Text
intercalateTail _ ["/"] = ""
intercalateTail sep ts = head ts <> Text.intercalate sep (tail ts)

parseFileOrDirName :: DirParser (Either File Text)
parseFileOrDirName = try $ Left <$> parseFile <|> Right <$> parseDirName

parseDirectory :: DirParser Directory
parseDirectory = do
  state <- getState
  let _dirName = head (state ^. currentDir)
      _dirPath = intercalateTail "/" (reverse $ state ^. currentDir)
  contents <- many parseFileOrDirName
  let subdirs = (\c -> _dirPath <> "/" <> c) <$> rights contents
  let parsedDir = Directory _dirName _dirPath (lefts contents) subdirs [] 0
      state' = case HashMap.lookup _dirPath (state ^. dirMap) of
        Just _ -> state
        Nothing -> state & dirMap %~ HashMap.insert _dirPath parsedDir
  putState state'
  return parsedDir

parseCommandDump :: DirParser (HashMap Text Directory)
parseCommandDump = do
  _ <- many1 parseCommand
  finalState <- getState
  pure (finalState ^. dirMap)

-- string' s matches string s, and up to as many whitespace tokens as it can get.
string' :: String -> DirParser String
string' s = string s <* spaces

resolveSubDirs :: HashMap Text Directory -> HashMap Text Directory
resolveSubDirs resMap = HashMap.map updateDir resMap
  where
    lookupSubdir = mapMaybe (`HashMap.lookup` resMap)
    recurseSubdir dir' =
      let subDirs = lookupSubdir (dir' ^. subDirNames)
       in subDirs <> concatMap recurseSubdir subDirs
    updateDir dir = dir & resolvedSubDirs .~ recurseSubdir dir

resolveSize :: Directory -> Directory
resolveSize dir = dir & resolvedSize .~ dirSize dir

dirSize :: Directory -> Integer
dirSize dir = foldr ((+) . (^. fileSize)) 0 (dir ^. files) + sum (dirSize <$> dir ^. resolvedSubDirs)

grabSmallestDeletableFolder :: HashMap Text Directory -> Directory
grabSmallestDeletableFolder dirs =
  let rootSpace = maybe 0 _resolvedSize (HashMap.lookup "" dirs)
      spaceToClear = requiredForUpdate - (totalSpace - rootSpace)
      largeFolders = HashMap.filter ((>= spaceToClear) . _resolvedSize) dirs
   in minimumBy (compare `on` _resolvedSize) largeFolders

directorySurfingLog :: IO Text
directorySurfingLog = Text.IO.readFile "./files/DaySeven/DaySeven.txt"

main :: IO ()
main = do
  directorySurfing <- directorySurfingLog
  let parseResult = dirParse parseCommandDump "" directorySurfing
  dirs <- case parseResult of
    Left err -> putStrLn ("parse err: " <> show err) >> pure HashMap.empty
    Right directories -> return $ resolveSize <$> resolveSubDirs directories
  let acceptableFolders = HashMap.filter ((<= unacceptableSizeBound) . _resolvedSize) dirs
  putStrLn ("QA: " <> show (sum $ _resolvedSize <$> acceptableFolders))
  putStrLn ("QB: " <> show (_resolvedSize $ grabSmallestDeletableFolder dirs))
