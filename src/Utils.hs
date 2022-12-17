module Utils
  ( number,
    string',
    tshow,
    tReadMaybe,
    chunksOf,
    splitAtMay,
  )
where
import Data.Text (Text)
import Data.Text qualified as Text
import Text.Parsec (digit, many1, string, spaces, try, (<|>))
import Text.Parsec.Text (Parser)
import Text.Read (readMaybe)
import Data.Maybe (fromMaybe)

number :: Parser Int
number = try readNegative <|> readPositive
  where 
    readPositive = read <$> many1 digit
    readNegative = negate . read <$ string "-" <*> many1 digit

string' :: String -> Parser String
string' s = string s <* spaces

tshow :: Show a => a -> Text
tshow = Text.pack . show

tReadMaybe :: Read a => Text -> Maybe a
tReadMaybe = readMaybe . Text.unpack

chunksOf :: Int -> [a] -> [[a]]
chunksOf 1 s = [s]
chunksOf n xs = fromMaybe [] $ do
  (chunk, remainder) <- splitAtMay n xs
  return $ chunk : chunksOf n remainder
  
splitAtMay :: Int -> [a] -> Maybe ([a], [a])
splitAtMay n xs = if length xs >= n then Just (splitAt n xs) else Nothing
