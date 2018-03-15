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

nix-shell --show-trace -p "(haskell.packages.${COMPILER}.override { overrides = self: super: { spiros = haskell.lib.dontCheck (haskell.lib.doHaddock (self.callCabal2nix ''spiros'' ./. {})); }; }).ghcWithPackages (self: with self; [ spiros ])"

########################################

# e.g.
# 
# Using install prefix: /nix/store/ky3684j19g301fwzy7yi53h0z8qmizqm-spiros-0.2
#
# Executables installed in:
# /nix/store/<hash>-spiros-0.2/bin
#
# Libraries installed in:
# /nix/store/<hash>-spiros-0.2/lib/ghc-8.2.2/spiros-0.2
#
# Dynamic Libraries installed in:
# /nix/store/<hash>-spiros-0.2/lib/ghc-8.2.2/x86_64-linux-ghc-8.2.2
#
# Private executables installed in:
# /nix/store/<hash>-spiros-0.2/libexec/x86_64-linux-ghc-8.2.2/spiros-0.2
#
# Data files installed in:
# /nix/store/<hash>-spiros-0.2/share/x86_64-linux-ghc-8.2.2/spiros-0.2
#
# Documentation installed in:
# /nix/store/<hash>-spiros-0.2/share/doc/x86_64-linux-ghc-8.2.2/spiros-0.2
#
# Configuration files installed in:
# /nix/store/<hash>-spiros-0.2/etc
#

