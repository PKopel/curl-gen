{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE CPP #-}

module Run
  ( run
  ) where

import           App                            ( App(appOptions)
                                                , Options(..)
                                                , ScriptLang(..)
                                                , ScriptOptions(..)
                                                )
import           Bash.Function                 as BashF
                                                ( writeFunction )
import           Bash.Template                 as BashT
                                                ( script )
import           Data.Attoparsec.Text           ( parseOnly )
import           Parser                         ( file )
import           Powershell.Function           as PwshF
                                                ( writeFunction )
import           Powershell.Template           as PwshT
                                                ( script )
import           RIO
import           RIO.List                       ( sort )
import           RIO.Text                       ( unpack )
import           System.IO                      ( putStrLn )
import           System.Info                    ( os )
#ifndef mingw32_HOST_OS
import           System.Posix.Files             ( ownerModes
                                                , setFileMode
                                                )
#endif
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

generators :: ScriptOptions -> (([Text], Curl) -> Text, [Text] -> Text)
generators opts = case lang opts of
  Bash       -> bash
  Powershell -> pwsh
  OsDefault  -> case os of
    "mingw32" -> pwsh
    _         -> bash
 where
  bash = (BashF.writeFunction (random opts), BashT.script opts)
  pwsh = (PwshF.writeFunction opts, PwshT.script opts)


run :: RIO App ()
run = do
  contents   <- view (to appOptions) <&> filePath >>= readFileUtf8
  scriptOpts <- view (to appOptions) <&> scriptOptions
  outFun     <- view (to appOptions) <&> outputPath <&> \case
    ""    -> putStrLn . unpack
    other -> \txt -> do
      writeFileUtf8 other txt
#ifndef mingw32_HOST_OS
      setFileMode other ownerModes
#endif
  let (functionGen, scriptGen) = generators scriptOpts
  liftIO $ case parseOnly file contents >>= mapM (secondM curlCmd) of
    Left  s  -> putStrLn s
    Right cu -> outFun . scriptGen $ map functionGen cu



