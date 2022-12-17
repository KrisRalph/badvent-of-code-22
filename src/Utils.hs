module Utils
  ( module Utils.Grid2d,
    number,
    string',
    tshow,
    tReadMaybe,
  )
where
import Utils.Grid2d 
import Data.Text (Text)
import Data.Text qualified as Text
import Text.Parsec (digit, many1, string, spaces)
import Text.Parsec.Text (Parser)
import Text.Read (readMaybe)

number :: Parser Int
number = read <$> many1 digit

string' :: String -> Parser String
string' s = string s <* spaces

tshow :: Show a => a -> Text
tshow = Text.pack . show

tReadMaybe :: Read a => Text -> Maybe a
tReadMaybe = readMaybe . Text.unpack
