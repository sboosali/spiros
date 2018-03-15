#!/bin/sh
set -e
########################################

# e.g.
# 
# ./haddock.sh --hyperlink-source --all --internal --hoogle
#
# ./haddock.sh --for-hackage --hyperlink-source 

########################################

cabal haddock --hyperlink-source "$@"

xdg-open ./dist/doc/html/spiros/index.html >/dev/null 2>&1

########################################

# nix-shell --run "cabal haddock \"$@\""
