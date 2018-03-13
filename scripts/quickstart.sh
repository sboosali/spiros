#!/bin/bash
set -e
########################################

ARGUMENTS="$@"

########################################

nix-shell --show-trace  -p '(haskell.packages.ghc822.override { overrides = self: super: { spiros = haskell.lib.dontCheck (haskell.lib.dontHaddock (self.callCabal2nix "spiros" ./spiros {})); }; }).ghcWithPackages (self: with self; [ spiros ])'

########################################

read -r -d '' INFORMATION <<EOF

:set -package spiros
:set -XNoImplicitPrelude
:set -XOverloadedStrings

EOF

echo -e "${INFORMATION}"

ghci  -package spiros  -XNoImplicitPrelude  -XOverloadedStrings

########################################

read -r -d '' EXAMPLES <<EOF

:load Examples_spiros
:info Example.WarningValidation
example_validateNaturalRatio 2 3

EOF

echo -e "${EXAMPLES}"

########################################

# haskell.packages.ghc822.ghcWithPackages (ps: with ps; [spiros])

#  (pkgs.haskellPackages.override { overrides = self: super: { spiros = self.callCabal2nix ./spiros {}; }; }).ghcWithPackages (ps: with ps; [ spiros ])
