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

options :: String -> IO (Options, ())
options version = simpleOptions
  version
  ("curl-gen " <> version)
  "Generate bash scripts from curl commands"
  (   Options
  <$> strArgument (metavar "FILE" <> help "File containing curl commands.")
  <*> (   ScriptOptions
      <$> switch
            (long "threads" <> short 't' <> help
              "Make multi-threaded script."
            )
      <*> switch
            (long "random" <> short 'r' <> help
              "Include code for random values."
            )
      )
  <*> switch (long "verbose" <> short 'v' <> help "Verbose output?")
  <*> strOption
        (  long "output"
        <> short 'o'
        <> metavar "OUTPUT_FILE"
        <> value ""
        <> help "Path to output"
        )
  )
  empty

main :: IO ()
main = do
  let version = $(simpleVersion Paths_curl_gen.version)
  (ops, ()) <- options version
  lo        <- logOptionsHandle stderr (verbose ops)
  pc        <- mkDefaultProcessContext
  withLogFunc lo $ \lf ->
    let app = App { appLogFunc = lf, appProcessContext = pc, appOptions = ops }
    in  runRIO app run
