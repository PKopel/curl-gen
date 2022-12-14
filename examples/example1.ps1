#!/usr/bin/env pwsh
# generated by curl-gen (https://github.com/PKopel/curl-gen)
 
param (
    [Parameter(
        Position = 0,
        ValueFromRemainingArguments = $true
    )]
    [string[]]$command,
    
    [switch]$dryRun,
    
    [string]$addr,
    
    [string]$file,
    
    [string[]]$set,
    
    [string[]]$path
)

function Set-Values {
    param (
        [string[]]$set,
    
        [string[]]$path
    )
    
    $values = @{}
    
    foreach ($x in $set + $path) {
        $field, $value = $x -split "="
        $values[$field] = "$value"
    }
    
    return [hashtable]$values
}

function Set-Object {
    param (
        [Parameter(Mandatory)]
        [hashtable]$values,
    
        [Parameter(Mandatory, ValueFromPipeline)]
        [hashtable]$object
    )
    
    function Set-Field {
        param (
            [string[]]$fs,
            [string]$val,
            [hashtable]$obj
        )
    
        if ($fs.count -gt 1) {
            $f, $fs = $fs
            $obj[$f] = Set-Field -fs $fs -val $val -obj $obj[$f]
        }
        else {
            $f = $fs[0]
            if ($f.Contains('[')) {
                # turn 'array[int]' into array and int
                $a, [int]$i, $_ = $f -split { $_ -in "[", "]" }
                $obj[$a][$i] = Invoke-Expression $val
            }
            else {
                $obj[$f] = Invoke-Expression $val
            }
        }
        return $obj
    }
    
    foreach ($key in $values.keys) {
        # drop leading .
        $_, $fields = $key -split { $_ -eq "." }
        if ($fields.count -gt 0) {
            $object = Set-Field -fs $fields -val $values[$key] -obj $object
        }
    }
    
    return $object
}

################### auto-generated functions start ###################
    
 
function test-example {
    param (
        [switch]$dryRun,
    
        [string]$addr = "localhost:8008",
    
        [string]$file,
    
        [string[]]$set,
    
        [string[]]$path
    )
    
    $values = Set-Values -set $set -rand $rand -path $path
    
    $url = "http://$addr/path"
    
    $data = '{"obj":{"string":"data"},"array":[1,null]}' | ConvertFrom-Json -AsHashtable 
        | Set-Object -values $values
        | ConvertTo-Json
    
    $opts = @(
 "-v"
"-k"
"-X PUT"

 "--header 'Accept: application/json'"

 "$url"
 "--data '$data'"
    )
    
    if ($dryRun.IsPresent) {
        $opts = $opts -join " "
        Write-Output "curl $opts"
    }
    else {
        Invoke-Expression "curl $opts"
    }
}

    
#################### auto-generated functions end ####################
 

if ($MyInvocation.InvocationName -ne '.') {
    $cmd = $command -join "-"
    Invoke-Expression "$cmd @PsBoundParameters"
}
