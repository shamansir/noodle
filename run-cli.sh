#!/bin/bash
if [ $# -ge 1 ]; then
    # spago run --exec-args "$@"
    spago run -m Cli.Main --exec-args "$1 $2 $3 $4 $5 $6 $7 $8 $9"
else
    spago run -m Cli.Main
fi