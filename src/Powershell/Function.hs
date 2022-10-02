{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}

module Powershell.Function
    ( writeFunction
    ) where

import           RIO                            ( (.)
                                                , (<>)
                                                , Text
                                                , map
                                                )
import           RIO.Text                       ( intercalate
                                                , unlines
                                                )
import           RIO.Text.Partial               ( replace )
import           Types.Curl                     ( Curl(..)
                                                , Dta(..)
                                                , Header(H)
                                                , URL(..)
                                                )
import           Types.Script                   ( Generator
                                                , ScriptOptions(..)
                                                )

writeFunction :: ScriptOptions -> Generator
writeFunction opts (txts, c) = "\n\
\function " <> intercalate "-" txts <> " {\n\
\    param (\n\
\        [switch]$dryRun,\n\
\    \n\
\        [string]$addr = \"" <> host (url  c) <> "\",\n\
\    " <> (if threads opts then thrd else "") <> "\n\
\        [string]$file,\n\
\    \n\
\        [string[]]$set,\n\
\    " <> (if random opts then rand else "") <> "\n\
\        [string[]]$path\n\
\    )\n\
\    \n\
\    $values = Set-Values -set $set -rand $rand -path $path\n\
\    " <> (if threads opts then withThreads else withoutThreads) c <> "\n\
\}"
  where
    rand = "\n\
\    [string[]]$rand,\n\
\ " :: Text
    thrd ="\n\
\    [int]$threads = 1,\n\
\ " :: Text

withThreads :: Curl -> Text
withThreads c@(Curl (URL pr _ ph) _ _ d) = "\n\
\    $setObject = ${function:Set-Object}.ToString()\n\
\    \n\
\    1..$threads | ForEach-Object -Parallel {\n\
\        ${function:Set-Object} = $using:setObject\n\
\    \n\
\        $url = \"" <> pr <> "://$using:addr" <> writePath ph <> "\"\n\
\    \n\
\        $data = " <> writeData d <> "\n\
\    \n\
\        $opts = " <> writeOpts c <> "\n\
\    \n\
\        if ($using:dryRun.IsPresent) {\n\
\            $opts = $opts -join \" \"\n\
\            Write-Output \"curl $opts\"\n\
\        }\n\
\        else {\n\
\            Invoke-Expression \"curl $opts\"\n\
\        }\n\
\    } -AsJob | Wait-Job | Receive-Job"
  where
    writeData NoData = "''" :: Text
    writeData (D v) = "'" <> v <> "' | ConvertFrom-Json -AsHashtable \n\
\        | Set-Object -values $using:values\n\
\        | ConvertTo-Json"
    writePath = replace "}" "'])" . replace "{" "$($using:values['"

withoutThreads :: Curl -> Text
withoutThreads c@(Curl (URL pr _ ph) _ _ d) = "\n\
\    $url = \"" <> pr <> "://$addr" <> writePath ph <> "\"\n\
\    \n\
\    $data = " <> writeData d <> "\n\
\    \n\
\    $opts = " <> writeOpts c <> "\n\
\    \n\
\    if ($dryRun.IsPresent) {\n\
\        $opts = $opts -join \" \"\n\
\        Write-Output \"curl $opts\"\n\
\    }\n\
\    else {\n\
\        Invoke-Expression \"curl $opts\"\n\
\    }"
  where
    writeData NoData = "''" :: Text
    writeData (D v) = "'" <> v <> "' | ConvertFrom-Json -AsHashtable \n\
\        | Set-Object -values $values\n\
\        | ConvertTo-Json"
    writePath = replace "}" "'])" . replace "{" "$($values['"

writeOpts :: Curl -> Text
writeOpts (Curl _ os hs _) = "@(\n\
\ " <> unlines (map opsLine os) <> "\n\
\ " <> unlines (map hdrLine hs) <> "\n\
\ \"$url\"\n\
\ \"--data '$data'\"\n\
\    )"
  where
    opsLine o = "\"" <> o <> "\""
    hdrLine (H h) = "\"--header '" <> h <> "'\""


