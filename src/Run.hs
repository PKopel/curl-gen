{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Run
  ( run
  ) where

import           App
import           Bash.Function                  ( writeFunction )
import           Data.Attoparsec.Text    hiding ( D )
import           Parser
import           RIO
import           RIO.List
import           RIO.Text                       ( unpack )
import           System.IO                      ( print
                                                , putStrLn
                                                )
import           Types
import           Util                           ( secondM )

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
  liftIO $ case parseOnly file contents >>= mapM (secondM curlCmd) of
    Left  s  -> print s
    Right cu -> putStrLn . unwords $ map (unpack . writeFunction) cu


