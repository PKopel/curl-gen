#!/usr/bin/env bash
# generated by curl-gen


function print_usage() {
    echo -e "Usage: $(basename $0) <command> [<options>]
Options:
    -h | --help
            print this message

    -t | --threads <number>
            run <number> concurrent threads.

    --dry-run
            display curl command instead of running it

    -f | --file <file path>
            use contents of <file path> as call data

    --rand <paths=types>
            replace fields in data object with random values.
            Supported types are I for integers and S for strings.

    --set <paths=values>
            replace fields in data object with provided values, e.g.
            --set '.test="asdfgh"' sets field test of data object
            to value "asdfgh"
"
}


declare -A VALUES

# usage: read_values <path> <value>;
function read_values() {
    local FIELD_PATH=$(echo $1 | sed 's/\[/\(/g' - | sed 's/\]/\)/g' - )
    VALUES["$FIELD_PATH"]="$2"
}


# usage: generate_values <path> <type>;
function generate_values() {
    local FIELD_PATH=$(echo $1 | sed 's/\[/\(/g' - | sed 's/\]/\)/g' - )
    local TYPE=$2
    local GENERATOR
    if [[ $TYPE == S ]]; then
        GENERATOR='echo \"$(cat /dev/random | tr -dc "[:alnum:]" | head -c 20)\"'
    elif [[ $TYPE == I ]]; then
        GENERATOR='od -vAn -N2 -tu2 < /dev/random'
    else
        echo "wrong type: $TYPE for $FIELD_PATH"
    fi
    VALUES["$FIELD_PATH"]="$GENERATOR"
}



################### auto-generated functions start ###################


function test_put() {
    local HOST=${ADDRESS:-'test.com'}
    local DATA
    if [[ -n "${FILE_PATH}" ]]; then
        DATA="$(cat ${FILE_PATH})"
    else
        DATA='{
    "obj":'${VALUES[".obj"]:-'{
        "string":'$(eval ${VALUES[".obj.string"]:-'echo \"data\"'})'
    }'}',
    "array":'${VALUES[".array"]:-'[
        '$(eval ${VALUES[".array(0)"]:-'echo 1.0'})',
        '$(eval ${VALUES[".array(1)"]:-'echo null'})'
    ]'}'
}'
    fi
    $CURL -k  -v -X  PUT \
        "https://$HOST/${PATH_PARAMS['--path']}" \
        --data "$DATA" \
        --header "Accept: application/json"
}


function test_get() {
    local HOST=${ADDRESS:-'test.com'}
    local DATA
    if [[ -n "${FILE_PATH}" ]]; then
        DATA="$(cat ${FILE_PATH})"
    else
        DATA=''
    fi
    $CURL -k  -v -X  GET \
        "https://$HOST/${PATH_PARAMS['--path']}/${PATH_PARAMS['--id']}" \
     \
        --header "Accept: application/json"
}



#################### auto-generated functions end ####################


CURL=$(which curl)

COMMAND=()
ADDRESS="example.com"
THREADS=1
declare -A PATH_PARAMS

while [[ "$#" -gt 0 ]]; do
    OPTION="$1"
    shift
    case $OPTION in
    --dry-run)
        CURL="echo curl"
        ;;
    --help | -h)
        print_usage
        exit 0
        ;;
    --addr | -a)
        ADDRESS="$1"
        shift
        ;;
    --threads | -t)
        THREADS="$1"
        shift
        ;;
    --file | -f)
    	FILE_PATH="$1"
    	shift
    	;;
    --rand)
        while [[ "$1" && ! "$1" == -* ]]; do
            FIELD="$(echo $1 | cut -d= -f1)"
            TYPE="$(echo $1 | cut -d= -f2)"
            generate_values "$FIELD" "$TYPE"
            shift
        done
        ;;
    --set)
        while [[ "$1" && ! "$1" == -* ]]; do
            FIELD="$(echo $1 | cut -d= -f1)"
            VALUE="$(echo $1 | cut -d= -f2)"
            read_values "$FIELD" "$VALUE"
            shift
        done
        ;;
    --*)
        PATH_PARAMS["$OPTION"]="$1"
        shift
        ;;
    *)
        COMMAND+=("$OPTION")
    esac
done

FUNCTION=$( echo "${COMMAND[*]}" | sed 's| |_|g' - )

if [ "$(type -t "$FUNCTION")" == "function" ]; then
    for T in $(seq 1 $THREADS); do
        $FUNCTION &
    done
else
    echo "wrong command: $FUNCTION"
    print_usage
    exit 1
fi

