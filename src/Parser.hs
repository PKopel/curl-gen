{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
module Parser where

import           Data.Attoparsec.Combinator
                                         hiding ( option )
import           Data.Attoparsec.Text    hiding ( option
                                                , string
                                                )
import           Data.Text                     as T
import           Import                  hiding ( host
                                                , path
                                                )

skipSpaces :: Parser ()
skipSpaces = skipMany space


oneOf :: [Char] -> Parser Char
oneOf list = satisfy (`elem` list)

noneOf :: [Char] -> Parser Char
noneOf list = satisfy (`notElem` list)

between :: Char -> Parser String
between c = char c *> many (satisfy (/= c)) <* char c

string :: Parser Text
string = skipSpaces *> between '\'' <|> between '"' <&> T.pack

host :: Parser Text
host = many (letter <|> digit <|> char '.') <&> T.pack

path :: Parser Text
path = many (letter <|> digit <|> oneOf "/?%=_.") <&> T.pack

url :: Parser URL
url = URL <$> ("http" <|> "https" <* "://") <*> host <*> path

header :: Parser Header
header = skipSpaces *> ("-H" <|> "--header") *> string <&> H

headers :: Parser [Header]
headers = many header

dta :: Parser Dta
dta = ("-d" <|> "--data") *> string

option :: Parser Option
option = skipSpaces *> withValue <|> withoutValue
  where
    name = (<>) <$> many1 (char '-') <*> many letter <&> T.pack
    cat a b = a <> " " <> b
    withValue    = cat <$> name <*> (skipSpaces *> string)
    withoutValue = name

options :: Parser [Option]
options = many option

curl :: Parser Curl
curl = Curl <$> ("curl" *> skipSpaces *> url) <*> options <*> headers <*> dta
