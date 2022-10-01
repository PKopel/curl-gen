{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Parser.File
  ( file
  ) where

import           Data.Attoparsec.Combinator
import           Data.Attoparsec.Text    hiding ( option
                                                , string
                                                )
import           RIO                     hiding ( words )
import           RIO.List.Partial               ( foldl1 )
import           RIO.Text                       ( pack
                                                , words
                                                )
import           Types.Curl                     ( Argument(..)
                                                , URL(URL)
                                                )

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
string = skipSpaces *> between '\'' <|> between '"' <&> pack

host :: Parser Text
host = many (letter <|> digit <|> oneOf "$._:") <&> pack

path :: Parser Text
path = many (letter <|> digit <|> oneOf "$/?%=_.{}") <&> pack

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
  name  = (<>) <$> many1 (char '-') <*> many letter <&> pack
  value = (:) <$> noneOf "-" <*> many1 (noneOf " ") <&> pack
  param = P <$> name <*> (skipSpaces *> (url <!> (string <|> value)))
  flag  = F <$> name
  arg   = A <$> url

command :: Parser [Argument]
command = "curl" *> many argument

curl :: Parser ([Text], [Argument])
curl = (,) <$> ids <*> cmd
 where
  ids = char '#' *> takeTill (== '\n') <* char '\n' <&> words
  cmd = skipSpaces *> command <* option ' ' (char '\n')

file :: Parser [([Text], [Argument])]
file = many (skipMany space *> curl)
