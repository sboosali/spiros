#!/bin/bash
########################################
# define variables

GHCID_FILE=./ghcid.txt

CABAL_TARGET="${1:-all}"

CABAL_COMMAND=(cabal new-repl "$CABAL_TARGET")

NIX_COMMAND=(nix-shell --run '"'"${CABAL_COMMAND[@]}"'"')

GHCID_COMMAND=(ghcid -o "$GHCID_FILE" --command "'""${NIX_COMMAND[@]}""'")

# ^ to reify each layer of nested commands, use [1] bash arrays and [2] alternating single-versus-double quotations. 

########################################
# print info

echo 
echo '[CABAL_COMMAND]'
echo "${CABAL_COMMAND[@]}"
echo 
echo '[NIX_COMMAND]'
echo "${NIX_COMMAND[@]}"
echo 
echo '[GHCID_COMMAND]'
echo "${GHCID_COMMAND[@]}"
echo

########################################
# run it

echo '...' > "$GHCID_FILE"
# emacsclient "$GHCID_FILE" &

eval "${GHCID_COMMAND[@]}"

########################################
