{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ExtendedDefaultRules #-}

module Bash.Function
  ( writeFunction
  ) where

import           Data.Aeson                     ( Object
                                                , Value(..)
                                                , decode
                                                )
import           RIO                     hiding ( unwords )
import           RIO.ByteString.Lazy            ( fromStrict )
import           RIO.HashMap                   as HM
                                         hiding ( map )
import           RIO.Text                       ( intercalate
                                                , pack
                                                , unwords
                                                )
import           RIO.Text.Partial               ( replace )
import           RIO.Vector                    as V
                                         hiding ( map
                                                , zip
                                                )
import           Text.InterpolatedString.Perl6  ( qc )
import           Types
import           Util


writeFunction :: ([Text], Curl) -> Text
writeFunction (txts, c) = [qc|
function {intercalate "_" txts}() \{
    local HOST=$\{ADDRESS:-'{host (url c)}'}
    local DATA
    if [[ -n "$\{FILE_PATH}" ]]; then
        DATA="$(cat $\{FILE_PATH})"
    else
        DATA='{showData (dta c)}'
    fi
    {writeCurl c}
}
|]
 where
  wrap s = "{\n" <> s <> "\n}"
  showData dt = case dt of
    D d    -> maybe d (wrap . writeJsonObj 0 "" . HM.toList) (decodeText d)
    NoData -> ""

decodeText :: Text -> Maybe Object
decodeText = decode . fromStrict . encodeUtf8

writeJsonObj :: Int -> Text -> [(Text, Value)] -> Text
writeJsonObj n parent pairs =
  intercalate ",\n" $ map (writeJsonObjField (n + 1) parent) pairs

writeJsonObjField :: Int -> Text -> (Text, Value) -> Text
writeJsonObjField n parent (name, val) = case val of
  Object _ -> node
  Array  _ -> node
  _flat    -> leaf
 where
  node
    = [qc|{indent n}"{name}":'$\{VALUES["{fieldPath}"]:-'{writeJsonVal n fieldPath val}'}'|]
  leaf
    = [qc|{indent n}"{name}":'$(eval $\{VALUES["{fieldPath}"]:-'echo {writeJsonVal n fieldPath val}'})'|]
  fieldPath = parent <> "." <> name

writeJsonArray :: Int -> Text -> [(Integer, Value)] -> Text
writeJsonArray n parent pairs =
  intercalate ",\n" $ map (writeJsonArrayField (n + 1) parent) pairs

writeJsonArrayField :: Int -> Text -> (Integer, Value) -> Text
writeJsonArrayField n parent (idx, val) = case val of
  Object _ -> node
  Array  _ -> node
  _flat    -> leaf
 where
  node
    = [qc|{indent n}'$\{VALUES["{fieldPath}"]:-'{writeJsonVal n fieldPath val}'}'|]
  leaf
    = [qc|{indent n}'$(eval $\{VALUES["{fieldPath}"]:-'echo {writeJsonVal n fieldPath val}'})'|]
  fieldPath = parent <> "(" <> pack (show idx) <> ")"

writeJsonVal :: Int -> Text -> Value -> Text
writeJsonVal n fieldPath val = case val of
  Object hm ->
    "{\n" <> writeJsonObj n fieldPath (HM.toList hm) <> "\n" <> indent n <> "}"
  Array vec ->
    let zipped = zip [0 ..] (V.toList vec)
    in  "[\n" <> writeJsonArray n fieldPath zipped <> "\n" <> indent n <> "]"
  String txt -> "\\\"" <> txt <> "\\\""
  Number sci -> pack $ show sci
  Bool   b   -> pack $ show b
  Null       -> "null"

writeCurl :: Curl -> Text
writeCurl (Curl (URL p _ a) o hs dt) = intercalate
  " \\\n    "
  (fstLine : urlLine : dtaLine dt : map hdrLine hs)
 where
  fstLine = unwords ("$CURL" : o)
  urlLine = [qc|    "{p}://$HOST{writePath a}"|]
  dtaLine (D _)  = "    --data \"$DATA\""
  dtaLine NoData = ""
  hdrLine (H h) = [qc|    --header "{h}"|]

writePath :: Text -> Text
writePath = replace "}" "']}" . replace "{" "${PATH_PARAMS['--"
