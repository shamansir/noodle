spago bundle --module Web.Main
node ./node_modules/parcel/lib/bin.js build ./web/app.html --no-cache
node ./node_modules/parcel/lib/bin.js serve ./web/app.html --no-cache --port 15000
