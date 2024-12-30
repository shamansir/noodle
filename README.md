# README

## A bit outdated web state

(running over the previous version)

https://noodle.labs.jb.gg

## Running

From _starter_ toolkit:

`sh ./run-cli.sh -t starter`

### Run with commands from the file:

`sh ./run-cli.sh -f ./src/Demo/Toolkit/Starter/starter.v0.1.ndf`

### Run both with toolkit and commands from the file

Will rewrite previous node definitions if they intersect

`sh ./run-cli.sh -t starter -f ./src/Demo/Toolkit/Starter/starter.v0.1.ndf`

## Generate Toolkit Code

Generate code for _starter_ toolkit with `purescript-codegen` using the given definition file

`sh ./run-cli.sh -g ./src/Demo/Toolkit/Starter/starter.v0.1.ndf -t starter`

The code will be located in `./src/Demo/Toolkit/Starter` directory.

## Help

`sh ./run-cli.sh --help`

## Run tests

`sh ./test.sh`