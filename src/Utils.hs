module Utils where

import Text.Parsec
import Text.Parsec.Text

number :: Parser Int
number = read <$> many1 digit