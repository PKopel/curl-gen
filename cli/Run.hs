{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE CPP #-}

module Run
  ( run
  ) where

import           App                            ( App(appOptions)
                                                , Options(..)
                                                )
import           Parser.Curl                    ( parseCurl )
import           RIO
import           RIO.Text                       ( unpack )
import           System.IO                      ( putStrLn )
import           Generators                     ( generators )
#ifndef mingw32_HOST_OS
import           System.Posix.Files             ( ownerModes
                                                , setFileMode
                                                )
#endif


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



