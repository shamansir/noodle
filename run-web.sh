#!/bin/bash
spago-legacy bundle-app -m Web.Main --to web/app.js
parcel build web/app.html
parcel serve ./web/app.html
# spago-legacy bundle-app --watch -m Web.Main --to web/app.js &