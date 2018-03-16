#!/bin/bash

time nix-shell --run 'cabal new-test unit' # "$@"
