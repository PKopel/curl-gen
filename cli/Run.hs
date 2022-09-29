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
import           Parser.Curl                    ( parseCurl )
import           Powershell.Function           as PwshF
                                                ( writeFunction )
import           Powershell.Template           as PwshT
                                                ( script )
import           RIO
import           RIO.Text                       ( unpack )
import           System.IO                      ( putStrLn )
import           System.Info                    ( os )
#ifndef mingw32_HOST_OS
import           System.Posix.Files             ( ownerModes
                                                , setFileMode
                                                )
#endif
import           Types

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
  liftIO $ case parseCurl contents of
    Left  s  -> putStrLn s
    Right cu -> outFun . scriptGen $ map functionGen cu



