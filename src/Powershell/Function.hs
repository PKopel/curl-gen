{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ExtendedDefaultRules #-}

module Powershell.Function
    ( writeFunction
    ) where

import           RIO                            ( ($)
                                                , (.)
                                                , Text
                                                , map
                                                )
import           RIO.Text                       ( intercalate
                                                , unlines
                                                )
import           RIO.Text.Partial               ( replace )
import           Text.InterpolatedString.Perl6  ( q
                                                , qc
                                                )
import           Types.Curl                     ( Curl(..)
                                                , Dta(..)
                                                , Header(H)
                                                , URL(..)
                                                )
import           Types.Script                   ( Generator
                                                , ScriptOptions(..)
                                                )

writeFunction :: ScriptOptions -> Generator
writeFunction opts (txts, c) = [qc|
function {intercalate "-" txts} \{
    param (
        [switch]$dryRun,
    
        [string]$addr = "{host . url $ c}",
    { if threads opts then thrd else ""}
        [string]$file,
    
        [string[]]$set,
    { if random opts then rand else ""}
        [string[]]$path
    )

    $values = Set-Values -set $set -rand $rand -path $path
    {(if threads opts then withThreads else withoutThreads) c}
}
|]
  where
    rand =
        [q|
    [string[]]$rand,
|] :: Text
    thrd =
        [q|
    [int]$threads = 1,
|] :: Text

withThreads :: Curl -> Text
withThreads c@(Curl (URL pr _ ph) _ _ d) = [qc|
    $setObject = $\{function:Set-Object}.ToString()

    1..$threads | ForEach-Object -Parallel \{
        $\{function:Set-Object} = $using:setObject

        $url = "{pr}://$using:addr{writePath ph}"

        $data = {writeData d}

        $opts = {writeOpts c}

        if ($using:dryRun.IsPresent) \{
            $opts = $opts -join " "
            Write-Output "curl $opts"
        }
        else \{
            Invoke-Expression "curl $opts"
        }
    } -AsJob | Wait-Job | Receive-Job|]
  where
    writeData NoData = "''" :: Text
    writeData (D v) = [qc|'{v}' | ConvertFrom-Json -AsHashtable 
        | Set-Object -values $using:values
        | ConvertTo-Json|]
    writePath = replace "}" "'])" . replace "{" "$($using:values['"

withoutThreads :: Curl -> Text
withoutThreads c@(Curl (URL pr _ ph) _ _ d) = [qc|
    $url = "{pr}://$addr{writePath ph}"

    $data = {writeData d}

    $opts = {writeOpts c}

    if ($dryRun.IsPresent) \{
        $opts = $opts -join " "
        Write-Output "curl $opts"
    }
    else \{
        curl @opts
    }
|]
  where
    writeData NoData = "''" :: Text
    writeData (D v) = [qc|'{v}' | ConvertFrom-Json -AsHashtable 
        | Set-Object -values $values
        | ConvertTo-Json|]
    writePath = replace "}" "'])" . replace "{" "$($values['"

writeOpts :: Curl -> Text
writeOpts (Curl _ os hs _) = [qc|@(
{unlines $ map opsLine os}
{unlines $ map hdrLine hs}
"$url"
"--data '$data'"
    )|]
  where
    opsLine o = [qc|{o}|]
    hdrLine (H h) = [qc|"--header '{h}'"|]


