#!/bin/bash
set -e
# set -eu

########################################

# e.g.
# 
# ./install.sh '{test = true; bench = true; haddock = true; shared = true; static = true; }'
# 
# ./install.sh '{test = true; bench = false; haddock = false; shared = true; static = true; }'
# 
# ./install.sh '{test = true; bench = true; haddock = true; shared = true; static = true;  sharedExecutables = true; goldLinker = true; coverage = true; strip = true; deadCodeElimination = true; checkUnusedPackages = true; dwarfDebugging = true; strict = true; }'
# 
# ./install.sh '{}'
# 
# ./install.sh '{}' --arg packageDotNix ./nix/spiros.nix
# =
# nix-build shell.nix --show-trace --arg options '{}' --arg packageDotNix ./nix/spiros.nix
# 
# 

########################################

OPTIONS="$1"
shift 1

#ARGUMENTS=()
IFS=" " read -r -a ARGUMENTS <<< "$@"

#ARGUMENTS=("$@")
#ARGUMENTS=($@)
#ARGUMENTS="$@"

########################################

echo 
echo "[OPTIONS]" 
echo "$OPTIONS"
echo
echo "[ARGUMENTS]" 
echo "${ARGUMENTS[@]}"
echo

# rm result
# # symlink

nix-build shell.nix --show-trace "${ARGUMENTS[@]}" --arg options "$OPTIONS"
#TODO

########################################

echo 
echo "[RESULT]"
echo

find ./result/ -maxdepth 1 
echo
find ./result/ | wc -l

########################################

#NOTES
#
# In install.sh line 22:
# ARGUMENTS=($@)
#            ^-- SC2206: Quote to prevent word splitting, or split robustly with mapfile or read -a.
#
# In install.sh line 32:
# echo "[ARGUMENTS]" "$ARGUMENTS"
#                     ^-- SC2128: Expanding an array without an index only gives the first element.
#
# In install.sh line 35:
# nix-build shell.nix --show-trace --arg options "$1" ${ARGUMENTS[@]}
#                                                     ^-- SC2068: Double quote array expansions to avoid re-splitting elements.

# https://github.com/koalaman/shellcheck/wiki/SC2128
# 
# 
# To get all elements as separate parameters, use the index @ (and make sure to double quote). In the example, echo "${myarray[@]}" is equivalent to echo "foo" "bar".
#
# To get all elements as a single parameter, concatenated by the first character in IFS, use the index *. In the example, echo "${myarray[*]}" is equivalent to echo "foo bar".

# https://github.com/koalaman/shellcheck/wiki/SC2206
#
# Quote to prevent word splitting, or split robustly with mapfile or read -a.
# 
# Problematic code:
# array=( $var )
# 
# Correct code:
# 
# If the variable should be a single element:
# array=( "$var" )
# 
# If it's multiple lines, each of which should be an element:
# 
# # For bash
# mapfile -t array <<< "$var"
# 
# # For ksh
# printf '%s\n' "$var" | while IFS="" read -r line; do array+=("$line"); done
# 
# If it's a line with multiple words (separated by spaces, other delimiters can be chosen with IFS), each of which should be an element:
#
# # For bash
# IFS=" " read -r -a array <<< "$var"
#
# # For ksh
# IFS=" " read -r -A array <<< "$var"
#
# Instead, prefer explicitly splitting (or not splitting):
# If the variable should become a single array element, quote it.
# If you want to split into lines or words, use mapfile, read -ra and/or while loops as appropriate.
# 
# This prevents the shell from doing unwanted splitting and glob expansion, and therefore avoiding problems with data containing spaces or special characters.

