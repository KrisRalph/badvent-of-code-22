module Utils where

import Data.Text (Text)
import Data.Text qualified as Text
import Text.Parsec (digit, many1)
import Text.Parsec.Text (Parser)
import Text.Read (readMaybe)

number :: Parser Int
number = read <$> many1 digit

tshow :: Show a => a -> Text
tshow = Text.pack . show

tReadMaybe :: Read a => Text -> Maybe a
tReadMaybe = readMaybe . Text.unpack
