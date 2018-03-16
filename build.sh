#!/bin/bash
set -e
########################################

# e.g.
#
# ./build.sh
#
# ./build.sh new
# ./build.sh old
#
# ./build.sh 802
# ./build.sh js
#
# ./build.sh --argstr compiler ghc802
# ./build.sh --argstr compiler ghcjs
#
# ./build.sh old 841 --arg isProfiled true 
# ./build.sh new js  --arg isProfiled true 
#
# ./build.sh  --arg withHoogle true  --arg isProfiled true  --arg isTested true  --arg isBenchmarked true  --arg isDocumented true  --arg isHyperlinked true  --arg isDwarf true  --argstr whichLinker gold  --argstr whichObjectLibrary static
#
#

########################################

DEFAULT_COMMAND="cabal new-build"

COMMAND="$DEFAULT_COMMAND"

########################################

# aliases for cabal new-versus-old:

case "$1" in

new)
  COMMAND="cabal new-build"
  shift 1
  ;;

old)
  COMMAND="cabal build"
  shift 1
  ;;

*)
  ;;
esac

########################################

echo "$@"
echo

./environment.sh $@ --run "$COMMAND"

