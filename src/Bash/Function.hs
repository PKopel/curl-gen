{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

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
import           RIO.Vector                    as V
                                         hiding ( map
                                                , zip
                                                )
import           Types
import           Util

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
writeData (D d) = "local DATA='" <> showData <> "'"
 where
  wrap s = "{\n" <> s <> "\n}"
  showData = maybe d (wrap . writeJsonObj 0 "" . HM.toList) (decodeText d)

decodeText :: Text -> Maybe Object
decodeText = decode . fromStrict . encodeUtf8

writeJsonObj :: Int -> Text -> [(Text, Value)] -> Text
writeJsonObj n parent pairs =
  intercalate ",\n" $ map (writeJsonObjField (n + 1) parent) pairs

writeJsonObjField :: Int -> Text -> (Text, Value) -> Text
writeJsonObjField n parent (name, val) =
  indent n <> fieldName <> writeJsonVal n fieldPath val <> "'}'"
 where
  fieldName = "\"" <> name <> "\":'${VALUES[\"" <> fieldPath <> "\"]:-'"
  fieldPath = parent <> "." <> name

writeJsonArray :: Int -> Text -> [(Integer, Value)] -> Text
writeJsonArray n parent pairs =
  intercalate ",\n" $ map (writeJsonArrayField (n + 1) parent) pairs

writeJsonArrayField :: Int -> Text -> (Integer, Value) -> Text
writeJsonArrayField n parent (idx, val) =
  indent n <> fieldName <> writeJsonVal n fieldPath val <> "'}'"
 where
  fieldName = "'${VALUES[\"" <> fieldPath <> "\"]:-'"
  fieldPath = parent <> "(" <> pack (show idx) <> ")"

writeJsonVal :: Int -> Text -> Value -> Text
writeJsonVal n fieldPath val = case val of
  Object hm ->
    "{\n" <> writeJsonObj n fieldPath (HM.toList hm) <> "\n" <> indent n <> "}"
  Array vec ->
    let zipped = zip [0 ..] (V.toList vec)
    in  "[\n" <> writeJsonArray n fieldPath zipped <> "\n" <> indent n <> "]"
  String txt -> "\"" <> txt <> "\""
  Number sci -> pack $ show sci
  Bool   b   -> pack $ show b
  Null       -> "null"

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
