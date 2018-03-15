#!/bin/sh

# e.g.
#
# ./build.sh
#
# ./build.sh --argstr compiler ghc802
# ./build.sh --argstr compiler ghcjs
#
# ./build.sh  --arg withHoogle true  --arg isProfiled true  --arg isTested true  --arg isBenchmarked true  --arg isDocumented true  --arg isHyperlinked true  --arg isDwarf true  --argstr whichLinker gold  --argstr whichObjectLibrary static
#
#
nix-shell --show-trace --run "cabal new-build" "$@"

