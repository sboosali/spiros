##################################################
{ systemPackages
, haskellPackages
, haskellUtilities
}:

##################################################
let

spiros = import ./spiros.nix {

  inherit systemPackages haskellPackages haskellUtilities;

};

#------------------------------------------------#

in
##################################################
{

  inherit spiros;

}
##################################################