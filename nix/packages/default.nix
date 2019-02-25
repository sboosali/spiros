##################################################
{ systemPackages
, haskellPackages
, haskellUtilities

, static
, strip
}:

##################################################
let
#------------------------------------------------#

haskellPackages' = haskellPackages;

# haskellPackages' = haskellPackages.override {

#   overrides = self: super:

#     packages;

# };

#------------------------------------------------#

overrides = import ./overrides {

  inherit systemPackages haskellUtilities;

  haskellPackages = haskellPackages';

  inherit static strip;

};

#------------------------------------------------#

cabal2nix = import ./cabal2nix {

  haskellPackages = haskellPackages';

};

#------------------------------------------------#
#------------------------------------------------#

#TODO# overriden « haskellPackages' » such that (project-)local packages depend on each other.

packages = {

  inherit spiros;
  inherit spiros-base;

  inherit example-spiros;

};

#------------------------------------------------#

spiros =

  haskellUtilities.overrideCabal (cabal2nix.spiros) (overrides.spiros);

#------------------------------------------------#

spiros-base =

  null; #TODO#

#------------------------------------------------#

example-spiros =

  haskellUtilities.justStaticExecutables spiros;

#------------------------------------------------#
in
##################################################

packages

##################################################