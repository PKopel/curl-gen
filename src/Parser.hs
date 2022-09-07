{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
module Parser
  ( file
  ) where

import           Data.Attoparsec.Combinator
import           Data.Attoparsec.Text    hiding ( option
                                                , string
                                                )
import           Data.Text                     as T
                                         hiding ( foldl1 )
import           Import                  hiding ( host
                                                , path
                                                , url
                                                )
import           RIO.List.Partial               ( foldl1 )

protocols :: [Parser Text]
protocols = ["https", "http", "ftp"]

skipSpaces :: Parser ()
skipSpaces = skipMany ((char '\n' <!> space) <|> "\\\n" $> ' ')

oneOf :: [Char] -> Parser Char
oneOf = satisfy . inClass

oneOfT :: [Parser Text] -> Parser Text
oneOfT = foldl1 (<|>)

noneOf :: [Char] -> Parser Char
noneOf = satisfy . notInClass

between :: Char -> Parser String
between c = char c *> manyTill anyChar (char c)

string :: Parser Text
string = skipSpaces *> between '\'' <|> between '"' <&> T.pack

host :: Parser Text
host = many (letter <|> digit <|> oneOf "$._") <&> T.pack

path :: Parser Text
path = many (letter <|> digit <|> oneOf "$/?%=_.") <&> T.pack

url :: Parser URL
url =
  URL
    <$> (many (oneOf "'\"") *> oneOfT protocols)
    <*> ("://" *> host)
    <*> option "" (path <* many (oneOf "'\""))

infixl 3 <!>
(<!>) :: Parser b -> Parser a -> Parser a
bad <!> good = eitherP bad good >>= \case
  Right a -> return a
  Left  _ -> fail "bad"

argument :: Parser Argument
argument = skipSpaces *> (arg <|> param <|> flag)
 where
  name  = (<>) <$> many1 (char '-') <*> many letter <&> T.pack
  value = (:) <$> noneOf "-" <*> many1 (noneOf " ") <&> T.pack
  param = P <$> name <*> (skipSpaces *> url <!> (string <|> value))
  flag  = F <$> name
  arg   = A <$> url

command :: Parser [Argument]
command = "curl" *> many argument

curl :: Parser ([Text], [Argument])
curl = (,) <$> ids <*> cmd
 where
  ids = char '#' *> takeTill (== '\n') <* char '\n' <&> T.words
  cmd = skipSpaces *> command <* option ' ' (char '\n')

file :: Parser [([Text], [Argument])]
file = many (skipMany space *> curl)
