{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Run
  ( run
  ) where

import           Import

curlCmd :: [Argument] -> Curl
curlCmd = undefined

run :: RIO App ()
run = do
  logInfo "We're inside the application!"
