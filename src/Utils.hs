module Utils where

import Data.Text (Text)
import Data.Text qualified as Text
import Text.Parsec
import Text.Parsec.Text

number :: Parser Int
number = read <$> many1 digit

tshow :: Show a => a -> Text
tshow = Text.pack . show
