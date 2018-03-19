#!/bin/bash
set -e
########################################

cabal2nix . "$@" > default.nix

echo
cat default.nix
echo
ls -l default.nix
echo

########################################
