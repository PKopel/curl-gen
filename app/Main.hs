{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}

module Main
  ( main
  ) where

import           App
import           Options.Applicative.Simple
import qualified Paths_curl_gen
import           RIO
import           RIO.Process
import           Run

main :: IO ()
main = do
  let version = $(simpleVersion Paths_curl_gen.version)
  (options, ()) <- simpleOptions
    version
    ("curl-gen " <> version)
    "Generate bash scripts from curl commands"
    (   Options
    <$> strArgument (metavar "FILE" <> help "File containing curl commands.")
    <*> switch (long "verbose" <> short 'v' <> help "Verbose output?")
    )
    empty
  lo <- logOptionsHandle stderr (optionsVerbose options)
  pc <- mkDefaultProcessContext
  withLogFunc lo $ \lf ->
    let app = App { appLogFunc        = lf
                  , appProcessContext = pc
                  , appOptions        = options
                  }
    in  runRIO app run
