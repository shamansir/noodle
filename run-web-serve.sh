#!/bin/bash
spago bundle
parcel build web/app.html --no-cache --port 15000
parcel serve ./web/app.html --no-cache --port 15000
# spago-legacy bundle-app --watch -m Web.Main --to web/app.js &