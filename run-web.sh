#!/bin/bash
if [ $# -ge 1 ]; then
    # spago run --exec-args "$@"
    spago-legacy build -m Web.Main --exec-args "$1 $2 $3 $4 $5 $6 $7 $8 $9" --to web/app.js
else
    spago-legacy build -m Web.Main --to web/app.js
fi
parcel build web/app.html
parcel serve ./web/app.html