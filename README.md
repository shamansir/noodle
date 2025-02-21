# README

## A bit outdated web state

(running over the previous version)

https://noodle.labs.jb.gg

## Temprorary Warnings

* It is developed with PureScript `0.15.9` and spago `0.21.0`, but now I have updated to `spago.yaml`, but still include `spago.dhall` "just in case";
* I also drafted a `flake.nix` for `nix` packaging and running:
  * It is not yet isolated with `node_packages`, and current CLI interface a.k.a. TUI requires `chjj/blessed` library from `npm`;
  * It works on my M1 Pro machine :D;
  * With `purescript-overlay` it allows to select compiler and `spago`, and `spago build` and all the commands below should work there in theory;
  * `nix develop` works, where you can do `spago build` and `spago run`;
  * `nix build` works;
  * `nix run` is not yet working, something with the terminal output;

## Installing

### Nix way

`nix run github:shamansir/noodle --extra-experimental-features nix-command --extra-experimental-features flakes`

* `nix shell` should work
* `nix build` also should work

### Other way

* Latest `spago` should work (`0.93.42` for the moment)
  * However it is developed with `spago` `v0.21.0`
* `purs` compiler `v0.15.9`, it is not yet updated/checked on the latest version;
* `npm install` is needed;

## Running

From _starter_ toolkit:

`sh ./run-cli.sh -t starter`

### Run with commands from the file:

`sh ./run-cli.sh -f ./ndf/starter.v0.1.ndf`

### Run both with toolkit and commands from the file

Will rewrite previous node definitions if they intersect

`sh ./run-cli.sh -t starter -f ./ndf/starter.v0.1.ndf`

## Generate Toolkit Code

Generate code for _starter_ toolkit with `purescript-codegen` using the given definition file

`sh ./run-cli.sh -g ./ndf/starter.v0.1.ndf -t starter`

The code will be located in `./src/Demo/Toolkit/Starter` directory.

## Help

`sh ./run-cli.sh --help`

## Run tests

`sh ./test.sh`