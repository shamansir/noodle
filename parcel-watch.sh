#!/bin/bash
rm -Rf ./.parcel-cache && parcel serve --no-cache --dist-dir ./static ./index.html
# parcel dev/index.html --out-dir dev-dist --open