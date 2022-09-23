# curl-gen

Program generating bash scripts from lists of curl commands.

## Usage

1. Run `stack install` to build and install `curl-gen` executable.
2. Prepare a txt file with curl commands (like in [examples](examples))
3. Run `curl-gen <path to .txt file>` (writes results to stdout) or `curl-gen <path to .txt file> -o script.sh` ([example result](examples/example3.sh))
4. To learn more about options run `curl-gen --help`

### Options

- `-r|--random` (optional) Include random values generator in the output script
- `-t|--threads` (optional) Make the output script multi-threaded (able to run multiple calls at the same time)

### Input file format

```txt
# test example
curl -X PUT -k -v https://test.com/path \
    --header 'Accept: application/json' \
    --data '{"obj":{"string":"data"},"array":[1,null]}'
```

Contents of the comment at the beginning are used to identify this curl call in script,
i.e. in this example, to make above call one would run `script.sh test example`
