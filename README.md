# rpd-purs

1. `npm install`
2. `bower install`
3. `npm run spago:watch`
4. `npm run parcel:watch`

When bower packages fail to build:

1. `rm -rf ./bower_components`
2. `rm -rf ./.pulp-cache`
3. `rm -rf ./.psci_modules`
4. `rm -rf ./output` (may be this one is most important)
5. `bower cache clean`
6. `bower install --force`
7. `pulp build`

Note to thyself:

1. When using GitHub repo, always specify the branch or tag, i.e. `bower install 'git://github.com/Cordobo/ngQuill#master' --save`, or else `bower` falls back to the tagged version, which could be not what was expected.
