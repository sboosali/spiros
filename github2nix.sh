#!/bin/bash
set -e
########################################

URL="${1:?}"

SUBDIRECTORY="$2"

########################################

if [[ -z $2 ]]; then
 NAME=$(basename "$URL")
else
 NAME="$SUBDIRECTORY"
fi

FILE="$NAME.nix"

if [[ -z $2 ]]; then
 OPTIONS=
else
 OPTIONS="--subpath $SUBDIRECTORY"
fi

########################################
echo
echo '[OPTIONS]'
echo
echo "$OPTIONS"

echo
echo "[$FILE]"
echo
cabal2nix "$URL" $OPTIONS > "$FILE"
echo
cat "$FILE"

echo 
########################################

# e.g.
# 
# $ ./github2nix.sh https://github.com/haskell/cabal/ Cabal
# $ cat Cabal.json
# 
