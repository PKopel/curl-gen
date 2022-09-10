{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ExtendedDefaultRules #-}

module Bash.Template where

import Text.InterpolatedString.Perl6 (q)
import RIO (Text)

header :: Text
header = [q|
#!/usr/bin/env bash

SCRIPT_NAME=$(basename $0)
# text colours
RED='\033[0;31m'
GREEN='\033[0;32m'
NC='\033[0m'
|]

info :: Text
info = [q|
# usage: info <message>;
function info() {
    if [ -z $QUIET ]; then
        if [ -t 1 ]; then
            echo -e "${GREEN}[INF]${NC} $1"
        else
            echo -e "[INF] $1"
        fi
    fi
}
|]

erro :: Text
erro = [q|
# usage: erro <message>;
function erro() {
    if [ -t 1 ]; then
        echo -e "${RED}[ERR]${NC} $1"
    else
        echo -e "[ERR] $1"
    fi
    print_usage
    exit 1
}
|]

print_usage :: Text
print_usage = [q|
function print_usage() {
    echo -e "Usage: $SCRIPT_NAME <command> [-h|--help] [-q|--quiet] [--dry-run] [--set <values>] "
}
|]

read_values :: Text
read_values = [q|
declare -A VALUES

# usage: read_values <path> <value> ...;
function read_values() {
    local FIELD_PATH=$(echo $1 | sed 's/\[/\(/g' - | sed 's/\]/\)/g' - )
    while [[ "$#" -gt 0 ]]; do
        VALUES["$FIELD_PATH"]="$2"
        shift
        shift
    done
}
|]

main :: Text
main = [q|
CURL=$(which curl)

COMMAND=()
ADDRESS="example.com"

while [[ "$#" -gt 0 ]]; do
    OPTION="$1"
    shift
    case $OPTION in
    --dry-run)
        CURL="echo curl"
        ;;
    --quiet | -q)
        QUIET="true"
        ;;
    --help | -h)
        print_usage
        exit 0
        ;;
    --addr | -a)
        ADDRESS="$1"
        shift
        ;;
    --set)
        read_values $@
        break
        ;;
    *)
        COMMAND+=("$OPTION")
    esac
done

FUNCTION=$( echo "${COMMAND[*]}" | sed 's| |_|g' - )

if [ $(type -t "$FUNCTION") == function ]; then
    $FUNCTION
fi
|]

