{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ExtendedDefaultRules #-}

module Bash.Function
  ( writeFunction
  ) where

import           App                            ( ScriptOptions(..) )
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
import           Types                          ( Curl(Curl, dta, url)
                                                , Dta(..)
                                                , Header(H)
                                                , URL(URL)
                                                )

writeFunction :: ScriptOptions -> ([Text], Curl) -> Text
writeFunction opts (txts, c) = [qc|
function {intercalate "-" txts} \{
    param (
        [switch]$dryRun,
    
        [string]$addr = "test.com",
    { if threads opts then thrd else ""}
        [string]$file,
    
        [string[]]$set,
    { if random opts then rand else ""}
        [string[]]$path
    )

    $values = Set-Values -set $set -rand $rand -path $path

    $url = {writeUrl $ url c}

    $data = {writeData $ dta c}

    $opts = {writeOpts c}

    if ($dryRun.IsPresent) \{
        $opts = $opts -join " "
        Write-Output "curl $opts"
    }
    else \{
        curl @opts
    }
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

writeData :: Dta -> Text
writeData NoData = "''"
writeData (D d) = [qc|'{d}' | ConvertFrom-Json -AsHashtable 
    | Set-Object -values $values
    | ConvertTo-Json|]

writeOpts :: Curl -> Text
writeOpts (Curl _ os hs _) = [qc|@(
        {unlines os}
        {unlines $ map hdrLine hs}
    )|]
  where hdrLine (H h) = [qc|"    --header '{h}'"|]

writeUrl :: URL -> Text
writeUrl (URL pr _ ph) = [qc|"{pr}://$addr{writePath ph}"|]

writePath :: Text -> Text
writePath = replace "}" "'])" . replace "{" "$($values['"
