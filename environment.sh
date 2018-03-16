#!/bin/bash
set -e
########################################

# e.g.
#
# ./build.sh
#
# ./build.sh 802
# ./build.sh js
#
# ./build.sh --argstr compiler ghc802
# ./build.sh --argstr compiler ghcjs
#
# ./build.sh js --arg isProfiled true 
#
# ./build.sh  --arg withHoogle true  --arg isProfiled true  --arg isTested true  --arg isBenchmarked true  --arg isDocumented true  --arg isHyperlinked true  --arg isDwarf true  --argstr whichLinker gold  --argstr whichObjectLibrary static
#
#

########################################

# ghc7103, ghc802, ghc822, ghc841, ghcjs

DEFAULT_COMPILER="default"
# DEFAULT_COMPILER="ghc822"

COMPILER="$DEFAULT_COMPILER"
#COMPILER="${1}"
# bash default argument

DEFAULT_INTEGER_SIMPLE=false

INTEGER_SIMPLE="$DEFAULT_INTEGER_SIMPLE"

########################################

# aliases for nixpkgs' compiler identifiers:

case "$1" in
 
7.10|7.10.3)
  COMPILER="ghc7103"
  shift 1
  ;;
8.0|8.0.2)
  COMPILER="ghc802"
  shift 1
  ;;
8.2|8.2.2)
  COMPILER="ghc822"
  shift 1
  ;;
8.4|8.4.1)
  COMPILER="ghc841"
  shift 1
  ;;

7)
  COMPILER="ghc7103"
  shift 1
  ;;
8)
  COMPILER="ghc841"
  shift 1
  ;;

710|7103)
  COMPILER="ghc7103"
  shift 1
  ;;
80|802)
  COMPILER="ghc802"
  shift 1
  ;;
82|822)
  COMPILER="ghc822"
  shift 1
  ;;
84|841)
  COMPILER="ghc841"
  shift 1
  ;;

HEAD|head)
  COMPILER="ghcHEAD"
  shift 1
  ;;

j|js)
  COMPILER="ghcjs"
  shift 1
  ;;

jHEAD|jhead|jsHEAD|jshead|ghcjshead)
  COMPILER="ghcjsHEAD"
  shift 1
  ;;

i)
  COMPILER="$DEFAULT_COMPILER"
  INTEGER_SIMPLE=true
  shift 1
  ;;

ghc7103)
  COMPILER="ghc7103"
  shift 1
  ;;
    
ghc802)
  COMPILER="ghc802"
  shift 1
  ;;
    
ghc822)
  COMPILER="ghc822"
  shift 1
  ;;

ghc841)
  COMPILER="ghc842"
  shift 1
  ;;
    
ghc841)
  COMPILER="ghc842"
  shift 1
  ;;
    
ghcHEAD)
  COMPILER="ghcHEAD"
  shift 1
  ;;
    
ghcjs)
  COMPILER="ghcjs"
  shift 1
  ;;
    
ghcjsHEAD)
  COMPILER="ghcjsHEAD"
  shift 1
  ;;

#TODO    
# integer-simple)
#   COMPILER="integer-simple"
#   shift 
#   ;;

i80|i802|i8.0.2|integer-simple-8.0.2)
  COMPILER="ghc801"
  INTEGER_SIMPLE=true
  shift 1
  ;;

i82|i822|i8.2.2|integer-simple|integer-simple-8.2.2)
  COMPILER="ghc822"
  INTEGER_SIMPLE=true
  shift 1
  ;;

i841|i8.4.1|integer-simple-8.4.1)
  COMPILER="ghc841"
  INTEGER_SIMPLE=true
  shift 1
  ;;

i84|i842|i8.4.2|integer-simple-8.4.2)
  COMPILER="ghc842"
  INTEGER_SIMPLE=true
  shift 1
  ;;

default)
  COMPILER="$DEFAULT_COMPILER"
  shift 1
  ;;

*)
  COMPILER="$DEFAULT_COMPILER"
  ;;

esac

########################################

echo "[COMPILER]" "$COMPILER" 
echo 
echo "[INTEGER-SIMPLE]" "$INTEGER_SIMPLE" 
echo 
echo "[OTHER ARGUMENTS]" "$@"
echo

nix-shell --show-trace --argstr compiler "$COMPILER" --arg integer-simple "$INTEGER_SIMPLE" "$@"

