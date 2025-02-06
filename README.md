# README

## A bit outdated web state

(running over the previous version)

https://noodle.labs.jb.gg

## Warnings

* It is developed with PureScript `0.15.9` and spago `0.21.0`, but now I have updated to `spago.yaml`, but still include `spago.dhall` "just in case";
* I also drafted a `flake.nix` for `nix` packaging and running:
  * It works on my M1 Pro machine with `nix develop --option system x86_64-darwin` (`develop` is not totally pure);
  * With `purescript-overlay` it allows to select compiler and `spago`, and `spago build` and all the commands below should work there in theory;
  * `nix build --option system x86_64-darwin` currently creates `result/output` but doesn't bundle it further in any way, it is in works;

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