#!/bin/bash
set -e
########################################

mkdir -p nix/

cabal2nix . > ./nix/spiros.nix

echo
echo '----------------------------------'
echo '[spiros.nix]'
echo 
cat ./nix/spiros.nix

echo
echo '----------------------------------'
echo '[default.nix]'
echo 
cat default.nix

echo
echo '----------------------------------'
echo '[provisioning...]'
echo
./provision.sh

echo
echo '----------------------------------'
echo '[building...]'
echo 
./build.sh

echo
echo '----------------------------------'
echo '[testing...]'
echo 
./test.sh

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
