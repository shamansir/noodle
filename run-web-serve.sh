#!/bin/bash
spago bundle
parcel build web/app.html --no-cache
parcel serve ./web/app.html --no-cache
# spago-legacy bundle-app --watch -m Web.Main --to web/app.js &