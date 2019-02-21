##################################################
{ systemPackages
, haskellPackages
, haskellUtilities

, strip
}:

##################################################
let

in
##################################################
let

overrides = import ./overrides {

  inherit systemPackages haskellPackages haskellUtilities;

};

#------------------------------------------------#

cabal2nix = import ./cabal2nix {

  inherit haskellPackages;

};

#------------------------------------------------#

spiros =

  haskellUtilities.overrideCabal (cabal2nix.spiros) (overrides.spiros);

#------------------------------------------------#

in
##################################################
{

  inherit spiros;

}
##################################################