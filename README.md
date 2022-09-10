# curl-gen

Program generating bash scripts from lists of curl commands.

## Usage

1. Run `stack install` to build and install `curl-gen` executable.
2. Prepare a txt file with curl commands (like in [examples](examples))
3. Run `curl-gen <path to .txt file>` (writes results to stdout) or `curl-gen <path to .txt file> -o script.sh`
4. To learn more about options run `curl-gen --help`
