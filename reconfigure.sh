#!/bin/bash
set -e
########################################

echo
echo '----------------------------------'
echo "[cabal2nix'ing...]"
echo 
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
echo '[default.nix (link)]'
echo 
readlink -f default.nix

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

echo
echo '----------------------------------'
echo '[generating dependencies metadata...]'
echo 
./dependencies.sh

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
