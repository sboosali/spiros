#!/bin/bash
set -e

# e.g.
# ./install.sh '{test = true; bench = true; haddock = true; shared = true; static = true; goldLinker = true; deadCodeElimination = true; checkUnusedPackages = true; }'

########################################

echo 
echo "[OPTIONS]" "$1"
echo
echo "[ARGUMENTS]" "$@"
echo

nix-build shell.nix --show-trace --arg options "$1" #TODO

########################################
