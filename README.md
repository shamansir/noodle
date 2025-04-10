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

## Running Terminal User Interface

`sh ./run-cli.sh -t starter`
`sh ./run-cli.sh -t hydra`

## Running Web Interface

`sh ./run-web-serve.sh`

For Hot Code Replacement, `sh ./run-web-watch.sh` could be run in parallel, but it sometimes doesn't update and in the modern `spago` versions this possibility was removed.

### Run with toolkit and commands from the file

Will rewrite previous node definitions if they intersect

`sh ./run-cli.sh -t starter -f ./ndf/starter.v0.1.ndf`
`sh ./run-cli.sh -t hydra -f ./ndf/hydra.v0.3.ndf`

## Generate Toolkit Code

Generate code for _starter_ toolkit with `purescript-codegen` using the given definition file

`sh ./run-cli.sh -g ./ndf/starter.v0.1.ndf -t starter`
`sh ./run-cli.sh -g ./ndf/hydra.v0.3.ndf -t hydra`

By default the generated code will be located in `./src/Demo/_Gen/Starter` directory (`./src/Demo/_Gen/<TOOLKIT-NAME>`). This could cause having double modules, though, i.e. fail to compile/start after the generation process, since generated modules for toolkit could have the exact same name as the toolkit sources already provided in the repository, or just the sources that were generated previously before current run. In this case ensure that you don't need the previous sources at this location or have them commited to version control, and delete the target directory before generating new ones.

Also, you could specify output directory manually:

`sh ./run-cli.sh -g ./ndf/starter.v0.1.ndf -t starter -o ~/Temp/_Codegen`

Notice that toolkit name will be appended to the path automatically, you don't need to specify it

## Command Bar

Command bar is called by `<TAB>` key and currently supports these kinds of `NDF` commands:

* `<family-name>` : spawn a node of given family
* `:: <inlets-spec> => <outlets-spec>` : spawn a node of given shape, some examples:
  * `:: <> => <>` : no inlets and no outlets
  * `:: <a:Number -> b:Number -> c:Number> => <>` : three inlets of given names and types
  * `:: <a -> b -> c> => <>` : just three inlets
  * `:: <r:Number -> g:Number -> b:Number> => <color:Color>` :  three inlets of given names and types + one outlet of given name and type
  * `:: <r:Number -> g:Number -> b:Number> => <color:Color {c/ff0000}>` :  three inlets of given names and types + one outlet of given name and type and default value
  * `:: <tuple> => <fst -> snd>` : one inlet and two outlets

## Help

`sh ./run-cli.sh --help`

## Run tests

`sh ./test.sh`

## Other

* `sh ./run-cli.sh --palette-test` : Test how color palette is visible in your terminal;
* `sh ./run-cli.sh --cli-demo` : Test if `chjj/blessed` interface works;