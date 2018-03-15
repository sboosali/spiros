#!/bin/bash
set -e
########################################

# ghc7103, ghc802, ghc822, ghc841, ghcjs

COMPILER="${1:-default}"
# bash default argument

case "$1" in

7.10)
  COMPILER="ghc7103"
  ;;
8.0)
  COMPILER="ghc802"
  ;;
8.2)
  COMPILER="ghc822"
  ;;
8.4)
  COMPILER="ghc841"
  ;;

7)
  COMPILER="ghc7103"
  ;;
8)
  COMPILER="ghc841"
  ;;

js)
  COMPILER="ghcjs"
  ;;

*)
  COMPILER="ghc822"
  ;;

esac

########################################

nix-shell --show-trace -p "(haskell.packages.${COMPILER}.override { overrides = self: super: { spiros = haskell.lib.dontCheck (haskell.lib.dontHaddock (self.callCabal2nix ''spiros'' ./. {})); }; }).ghcWithPackages (self: with self; [ spiros ])"

########################################
