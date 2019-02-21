##################################################
{ pkgs
, haskellPackages
, haskellUtilities

, strip
}:

##################################################
let

inherit (pkgs) lib;

#------------------------------------------------#

linkStatically = import ./lib/link-statically.nix {
  inherit pkgs haskellUtilities;
  inherit strip;
};

in
##################################################
let

the-cabal2nix-package = import ../cabal2nix/skeletor.nix;

#------------------------------------------------#

the-cabal2nix-derivation = haskellPackages.callPackage the-cabal2nix-package {

  spiros = haskellPackages.callCabal2nix "spiros" ~/haskell/spiros/spiros {};

};

#------------------------------------------------#

the-static-derivation = linkStatically the-cabal2nix-derivation;

in
##################################################

haskellUtilities.shellAware the-static-derivation

##################################################