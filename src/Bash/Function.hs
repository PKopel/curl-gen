{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}

module Bash.Function
  ( writeFunction
  ) where

import           Data.Aeson.Micro               ( Object
                                                , Value(..)
                                                , decode
                                                )
import           RIO                     hiding ( unwords )
import           RIO.ByteString.Lazy            ( fromStrict )
import           RIO.Map                       as M
                                                ( toList )
import           RIO.Text                       ( intercalate
                                                , pack
                                                , unwords
                                                )
import           RIO.Text.Partial               ( replace )
import           Types.Curl                     ( Curl(Curl, dta, url)
                                                , Dta(D, NoData)
                                                , Header(H)
                                                , URL(URL, host)
                                                )
import           Types.Script                   ( Generator )
import           Util                           ( indent )

data FunctionOptions = FunOpts !Bool !Int

writeFunction :: Bool -> Generator
writeFunction r (txts, c) =
  "\n\
\function "
    <> intercalate "_" txts
    <> "() {\n\
\    local HOST=${ADDRESS:-'"
    <> host (url c)
    <> "'}\n\
\    local DATA\n\
\    if [[ -n \"${FILE_PATH}\" ]]; then\n\
\        DATA=\"$(cat ${FILE_PATH})\"\n\
\    else\n\
\        DATA='"
    <> showData (dta c)
    <> "'\n\
\    fi\n\
\    "
    <> writeCurl c
    <> "\n\
\}"
 where
  opts = FunOpts r 0
  wrap s = "{\n" <> s <> "\n}"
  showData dt = case dt of
    D d    -> maybe d (wrap . writeJsonObj opts "" . M.toList) (decodeText d)
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
  node =
    indent n
      <> "\""
      <> name
      <> "\":'${VALUES[\""
      <> fieldPath
      <> "\"]:-'"
      <> writeJsonVal op fieldPath val
      <> "'}'"
  leaf =
    indent n
      <> "\""
      <> name
      <> "\":'$(eval ${VALUES[\""
      <> fieldPath
      <> "\"]:-'echo "
      <> writeJsonVal op fieldPath val
      <> "'})'"
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
  node =
    indent n
      <> "'${VALUES[\""
      <> fieldPath
      <> "\"]:-'"
      <> writeJsonVal op fieldPath val
      <> "'}'"
  leaf =
    indent n
      <> "'$(eval ${VALUES[\""
      <> fieldPath
      <> "\"]:-'echo "
      <> writeJsonVal op fieldPath val
      <> "'})'"
  fieldPath = parent <> "(" <> pack (show idx) <> ")"

writeJsonVal :: FunctionOptions -> Text -> Value -> Text
writeJsonVal op@(FunOpts r n) fieldPath val = case val of
  Object hm ->
    "{\n" <> writeJsonObj op fieldPath (M.toList hm) <> "\n" <> indent n <> "}"
  Array vec ->
    let zipped = zip [0 ..] vec
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
  urlLine = "    \"" <> p <> "://$HOST" <> writePath a <> "\""
  dtaLine (D _)  = "    --data \"$DATA\""
  dtaLine NoData = ""
  hdrLine (H h) = "    --header \"" <> h <> "\""

writePath :: Text -> Text
writePath = replace "}" "']}" . replace "{" "${VALUES['"
