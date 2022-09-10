{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Run
  ( run
  ) where

import           App
import           Bash.Function                  ( writeFunction )
import           Bash.Template                  ( script )
import           Data.Attoparsec.Text           ( parseOnly )
import           Parser                         ( file )
import           RIO
import           RIO.List                       ( sort )
import           RIO.Text                       ( unpack )
import           System.IO                      ( putStrLn )
import           System.Posix.Files             ( ownerModes
                                                , setFileMode
                                                )
import           Types
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



run :: RIO App ()
run = do
  contents <- view (to appOptions) <&> filePath >>= readFileUtf8
  outFun   <- view (to appOptions) <&> outputPath <&> \case
    ""    -> putStrLn . unpack
    other -> \txt -> do
      writeFileUtf8 other txt
      setFileMode other ownerModes
  liftIO $ case parseOnly file contents >>= mapM (secondM curlCmd) of
    Left  s  -> putStrLn s
    Right cu -> outFun . script $ map writeFunction cu



