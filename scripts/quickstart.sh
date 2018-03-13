#!/bin/bash
########################################

ARGUMENTS="$@"

########################################

read -r -d '' INFORMATION <<EOF

ghci
:set -package spiros
:set -XNoImplicitPrelude
:set -XOverloadedStrings
:m +Prelude.Spiros Examples_spiros
example_validateNaturalRatio 2 3
...

EOF

echo
echo -e "${INFORMATION}"
echo

########################################

nix-shell --show-trace  -p '(haskell.packages.ghc822.override { overrides = self: super: { spiros = haskell.lib.dontCheck (haskell.lib.dontHaddock (self.callCabal2nix "spiros" ./spiros {})); }; }).ghcWithPackages (self: with self; [ spiros ])'  --command "echo -e '\n:m +Prelude.Spiros Examples_spiros \nexample_validateNaturalRatio 2 3\n' ; ghci -package spiros -XNoImplicitPrelude -XOverloadedStrings " 

########################################

# haskell.packages.ghc822.ghcWithPackages (ps: with ps; [spiros])

#  (pkgs.haskellPackages.override { overrides = self: super: { spiros = self.callCabal2nix ./spiros {}; }; }).ghcWithPackages (ps: with ps; [ spiros ])

# nix-shell --show-trace  -p '(haskell.packages.ghc822.override { overrides = self: super: { spiros = haskell.lib.dontCheck (haskell.lib.dontHaddock (self.callCabal2nix "spiros" ./spiros {})); }; }).ghcWithPackages (self: with self; [ spiros ])' 

# nix-shell --show-trace  -p '(haskell.packages.ghc822.override { overrides = self: super: { spiros = haskell.lib.dontCheck (haskell.lib.dontHaddock (self.callCabal2nix "spiros" ./spiros {})); }; }).ghcWithPackages (self: with self; [ spiros ])'  --command 'ghci; return'

# echo -e '\n:m +Prelude.Spiros Examples_spiros'; echo -e '\nexample_validateNaturalRatio 2 3'


