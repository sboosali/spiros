#!/bin/bash

echo
echo ':m + Prelude.Spiros Examples_spiros'
echo

COMMAND='cabal new-repl spiros'
nix-shell --run "$COMMAND"

