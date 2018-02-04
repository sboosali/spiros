#!/bin/sh

# e.g.
#
# ./ghc802-build.sh
#

nix-shell --run "cabal build" --argstr compiler ghc802 "$@"
