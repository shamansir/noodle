#!/bin/bash
spago -x ./example.dhall build --watch && parcel ./example/Hydra/index.html # better run in different Terminals