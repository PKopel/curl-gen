#!/usr/bin/env bash

SCRIPT_NAME=$(basename $0)
# text colours
RED='\033[0;31m'
GREEN='\033[0;32m'
NC='\033[0m'

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

function print_usage() {
    echo -e "Usage: $SCRIPT_NAME <command> [-h|--help] [-q|--quiet] [--dry-run] [--set <values>] "
}

declare -A VALUES

# usage: read_values <path> <value> ...;
function read_values() {
    while [[ "$#" -gt 0 ]]; do
        VALUES["$1"]="$2"
        shift
        shift
    done
}


function example_get() {
    local HOST=${ADDRESS:-'example.com'}
    local DATA='{
    "test":'${VALUES[".test"]:-'{
        "data":'${VALUES[".test.data"]:-'1234'}'
    }'}'   
}'
    $CURL -X GET "https://${HOST}/test" \
        --header "Accept: application/json" \
        --data "$DATA"
}

###############################
########### REPLACE ###########
###############################

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

