#!/bin/bash
spago -x ./example.dhall build --watch && parcel --dist-dir ./static index.html