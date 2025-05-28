#!/bin/bash
if [ $# -ge 1 ]; then
    spago run -m Cli.Main --backend-args "$1 $2 $3 $4 $5 $6 $7 $8 $9"
else
    spago run -m Cli.Main
    # node /Users/shamansir/Workspace/noodle/.spago/run/run.js -t starter
fi