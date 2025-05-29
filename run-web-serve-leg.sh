#!/bin/bash
spago-legacy bundle-app -m Web.Main --to web/app.js --purs-args "-o ./output.legacy"
parcel build web/app.html --no-cache
parcel serve ./web/app.html --no-cache
# spago-legacy bundle-app --watch -m Web.Main --to web/app.js &