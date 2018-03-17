#!/bin/bash
set -e

# e.g.
# ./options-environment.sh '{test = true; bench = true; haddock = true; shared = true; static = true; goldLinker = true; deadCodeElimination = true; checkUnusedPackages = true; }'

########################################

echo 
echo "[OPTIONS]" "$@"
echo

nix-shell --show-trace --arg options "$@"

########################################
