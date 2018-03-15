#!/bin/sh

# e.g.
#
# ./ghc802-build.sh
#

nix-shell --run "cabal new-build" --argstr compiler ghc802 "$@"
