{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
module Run
  ( run
  ) where

import           Data.Attoparsec.Text    hiding ( D )
import           Import
import           Parser
import           RIO.List
import           System.IO                      ( print )

curlCmd :: [Argument] -> Either String Curl
curlCmd args = case sort args of
  (A u : o) -> curlOps o $ Curl u [] [] (D "")
  _         -> Left "No url"

curlOps :: [Argument] -> Curl -> Either String Curl
curlOps [] c = Right c
curlOps (P n v : args) c
  | n == "-d" || n == "--data" = curlOps args $ c { dta = D v }
  | n == "-H" || n == "--header" = curlOps args $ c { hds = H v : hds c }
  | otherwise = curlOps args $ c { ops = n <> " " <> v : ops c }
curlOps (F n : args) c = curlOps args $ c { ops = n : ops c }
curlOps (A _ : _   ) _ = Left "Too many urls"



run :: RIO App ()
run = do
  contents <- view (to appOptions) <&> filePath >>= readFileUtf8
  liftIO $ case parseOnly curl contents >>= curlCmd of
    Left  s  -> print s
    Right cu -> print cu


