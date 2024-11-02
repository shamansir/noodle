#!/bin/bash
#rm -Rf ./outuput && spago -x ./example.dhall build --watch && rm -Rf ./.parcel-cache && parcel serve --no-cache --out-dir ./static ./index.html
sh ./spago-watch.sh && sh ./parcel-watch.sh
