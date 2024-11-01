#!/bin/bash
if [ $# -ge 1 ]; then
    # spago run --exec-args "$@"
    spago run --exec-args "-f $1"
else
    spago run
fi
