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
                                                ( toList )
import           RIO.Text                       ( intercalate
                                                , pack
                                                , unwords
                                                )
import           RIO.Text.Partial               ( replace )
import           RIO.Vector                    as V
                                                ( toList )
import           Text.InterpolatedString.Perl6  ( qc )
import           Types                          ( Curl(Curl, dta, url)
                                                , Dta(D, NoData)
                                                , Header(H)
                                                , URL(URL, host)
                                                )
import           Util                           ( indent )

data FunctionOptions = FunOpts !Bool !Int

writeFunction :: Bool -> ([Text], Curl) -> Text
writeFunction r (txts, c) = [qc|
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
  opts = FunOpts r 0
  wrap s = "{\n" <> s <> "\n}"
  showData dt = case dt of
    D d    -> maybe d (wrap . writeJsonObj opts "" . HM.toList) (decodeText d)
    NoData -> ""

decodeText :: Text -> Maybe Object
decodeText = decode . fromStrict . encodeUtf8

writeJsonObj :: FunctionOptions -> Text -> [(Text, Value)] -> Text
writeJsonObj (FunOpts r n) parent pairs =
  intercalate ",\n" $ map (writeJsonObjField (FunOpts r $ n + 1) parent) pairs

writeJsonObjField :: FunctionOptions -> Text -> (Text, Value) -> Text
writeJsonObjField op@(FunOpts r n) parent (name, val) = case val of
  Object _ -> node
  Array  _ -> node
  _flat    -> if r then leaf else node
 where
  node
    = [qc|{indent n}"{name}":'$\{VALUES["{fieldPath}"]:-'{writeJsonVal op fieldPath val}'}'|]
  leaf
    = [qc|{indent n}"{name}":'$(eval $\{VALUES["{fieldPath}"]:-'echo {writeJsonVal op fieldPath val}'})'|]
  fieldPath = parent <> "." <> name

writeJsonArray :: FunctionOptions -> Text -> [(Integer, Value)] -> Text
writeJsonArray (FunOpts r n) parent pairs =
  intercalate ",\n" $ map (writeJsonArrayField (FunOpts r $ n + 1) parent) pairs

writeJsonArrayField :: FunctionOptions -> Text -> (Integer, Value) -> Text
writeJsonArrayField op@(FunOpts r n) parent (idx, val) = case val of
  Object _ -> node
  Array  _ -> node
  _flat    -> if r then leaf else node
 where
  node
    = [qc|{indent n}'$\{VALUES["{fieldPath}"]:-'{writeJsonVal op fieldPath val}'}'|]
  leaf
    = [qc|{indent n}'$(eval $\{VALUES["{fieldPath}"]:-'echo {writeJsonVal op fieldPath val}'})'|]
  fieldPath = parent <> "(" <> pack (show idx) <> ")"

writeJsonVal :: FunctionOptions -> Text -> Value -> Text
writeJsonVal op@(FunOpts r n) fieldPath val = case val of
  Object hm ->
    "{\n" <> writeJsonObj op fieldPath (HM.toList hm) <> "\n" <> indent n <> "}"
  Array vec ->
    let zipped = zip [0 ..] (V.toList vec)
    in  "[\n" <> writeJsonArray op fieldPath zipped <> "\n" <> indent n <> "]"
  String txt -> qt <> txt <> qt
  Number sci -> pack $ show sci
  Bool   b   -> pack $ show b
  Null       -> "null"
  where qt = if r then "\\\"" else "\""

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
writePath = replace "}" "']}" . replace "{" "${VALUES['"
