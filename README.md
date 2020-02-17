# noodle

Visual programming UI and framework driven by pure functional language and data streams.

## Installation

* `npm install`
* a) `npm run spago:watch`
* b) `npm run parcel:watch`

## Docker

* `npm run docker:build`
* `npm run docker:run`

## Development

Currently developed with VS Code and [PureScript IDE](https://github.com/nwolverson/vscode-ide-purescript.git) / [PureScript Language Support](https://github.com/nwolverson/vscode-language-purescript) plugins.

Configuration (pay attention to `spago` options, they are working for `spago v0.14`):

```json
{
    "purescript.addNpmPath": true,
    "purescript.pscIdeServerExe": "purs ide server",
    "purescript.addPscPackageSources": true,
    "purescript.pursExe": "/usr/local/bin/purs",
    "purescript.addSpagoSources": true,
    "purescript.autoStartPscIde": false,
    "purescript.buildCommand": "spago --quiet build --purs-args --json-errors"
}
```
