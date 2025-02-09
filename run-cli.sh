#!/bin/bash
if [ $# -ge 1 ]; then
    # spago run --exec-args "$@"
    spago-legacy run -m Cli.Main --exec-args "$1 $2 $3 $4 $5 $6 $7 $8 $9"
    # node /Users/shamansir/Workspace/noodle/.spago/run/run.js -t starter
else
    spago-legacy run -m Cli.Main
    # node /Users/shamansir/Workspace/noodle/.spago/run/run.js -t starter
fi