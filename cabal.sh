#!/bin/bash
set -e
########################################

# e.g.
#
# ./cabal.sh
#
# ./cabal.sh new build
# ./cabal.sh old build
# 
# ./cabal.sh new-repl
# ./cabal.sh repl
#
# ./cabal.sh 802
# ./cabal.sh js
#
# ./cabal.sh --argstr compiler ghc802
# ./cabal.sh --argstr compiler ghcjs
#
# ./cabal.sh old 841 --arg isProfiled true 
# ./cabal.sh new js  --arg isProfiled true 
#
# ./cabal.sh  --arg withHoogle true  --arg isProfiled true  --arg isTested true  --arg isBenchmarked true  --arg isDocumented true  --arg isHyperlinked true  --arg isDwarf true  --argstr whichLinker gold  --argstr whichObjectLibrary static
#
#

########################################

DEFAULT_COMMAND="cabal new-build"

COMMAND="$DEFAULT_COMMAND"

########################################

# aliases for cabal new-versus-old:
case "$1" in

new)
  SUBCOMMAND="${2:?}"
  COMMAND="cabal new-$SUBCOMMAND"
  shift 2
  ;;

old)
  SUBCOMMAND="${2:?}"
  COMMAND="cabal $SUBCOMMAND"
  shift 2
  ;;

build|repl|test|bench|run|haddock)
  SUBCOMMAND="${1}"
  COMMAND="cabal $SUBCOMMAND"
  shift 1
  ;;

new-build|new-repl|new-test|new-bench|new-run|new-haddock)
  SUBCOMMAND="${1}"
  COMMAND="cabal $SUBCOMMAND"
  shift 1
  ;;

*)
  ;;

esac

# case "$1" in

# new)
#   SUBCOMMAND="${2}"
#   COMMAND="cabal new-$SUBCOMMAND"
#   shift 1
#   ;;

# old)
#   SUBCOMMAND="${2}"
#   COMMAND="cabal $SUBCOMMAND"
#   shift 1
#   ;;

# build|repl|test|bench|run|haddock)
#   shift 1
#   ;;

# *)
#   ;;

# esac

########################################

echo "$@"
echo

./environment.sh $@ --run "$COMMAND"

