#!/bin/bash
rm -Rf ./outuput && spago -x ./example.dhall build --watch && rm -Rf ./.parcel-cache && parcel --dist-dir ./static index.html