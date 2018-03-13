#!/bin/bash
set -e
########################################

ARGUMENTS=

########################################

nix-shell --show-trace  -p '(haskell.packages.ghc841.override { overrides = self: super: { spiros = haskell.lib.dontCheck (haskell.lib.dontHaddock (self.callCabal2nix "spiros" ./. {})); }; }).ghcWithPackages (self: with self; [ spiros ])' 

########################################
