#!/bin/bash
set -e
########################################

cabal2nix . > ./default.nix

./provision.sh
./build.sh

########################################
#
# SHELL_FILE=shell-reflex-vinyl.nix
# cabal2nix . --shell > "$SHELL_FILE"
#
# if [ ! -f "$SHELL_FILE" ]; then 
#   # don't overwrite if it already exists
#   cabal2nix . --shell > "$SHELL_FILE" 
# fi
#
