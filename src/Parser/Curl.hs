{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Parser.Curl
    ( parseCurl
    ) where
import           Data.Attoparsec.Text           ( parseOnly )
import           Parser.File                    ( file )
import           RIO
import           RIO.List                       ( sort )
import           Types.Curl                     ( Argument(..)
                                                , Curl(..)
                                                , Dta(..)
                                                , Header(H)
                                                )
import           Util                           ( secondM )


curlCmd :: [Argument] -> Either String Curl
curlCmd args = case sort args of
    (A u : o) -> curlOps o $ Curl u [] [] NoData
    _         -> Left "No url"

curlOps :: [Argument] -> Curl -> Either String Curl
curlOps [] c = Right c
curlOps (P n v : args) c
    | n == "-d" || n == "--data" = curlOps args $ c { dta = D v }
    | n == "-H" || n == "--header" = curlOps args $ c { hds = H v : hds c }
    | otherwise = curlOps args $ c { ops = n <> " " <> v : ops c }
curlOps (F n : args) c = curlOps args $ c { ops = n : ops c }
curlOps (A _ : _   ) _ = Left "Too many urls"

parseCurl :: Text -> Either String [([Text], Curl)]
parseCurl contents = parseOnly file contents >>= mapM (secondM curlCmd)
