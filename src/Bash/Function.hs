{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Bash.Function
    ( writeFunction
    ) where

import           RIO                            ( (<>)
                                                , Text
                                                , map
                                                , undefined
                                                )
import           RIO.Text                       ( intercalate
                                                , unwords
                                                )
import           Types

writeFunction :: ([Text], Curl) -> Text
writeFunction (t, c) = intercalate
    "\n    "
    [ writeName t <> " {"
    , writeUrl (url c)
    , writeData (dta c)
    , writeCurl c <> "\n}\n"
    ]

writeName :: [Text] -> Text
writeName txts = "function " <> intercalate "_" txts <> "()"

writeData :: Dta -> Text
writeData = undefined

writeUrl :: URL -> Text
writeUrl u = "local HOST=${ADDRESS:-'" <> host u <> "'}"

writeCurl :: Curl -> Text
writeCurl (Curl (URL p _ a) o hs _) = intercalate
    " \\\n\t"
    (fstLine : urlLine : "--data \"$DATA\"" : map hdrLine hs)
  where
    fstLine = unwords ("$CURL" : o)
    urlLine = "\"" <> p <> "://$HOST" <> a <> "\""
    hdrLine (H h) = "--header \"" <> h <> "\""
