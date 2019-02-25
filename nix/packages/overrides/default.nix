##################################################
{ systemPackages
, haskellPackages
, haskellUtilities

, static
, strip
}:

##################################################
let

spiros = import ./spiros.nix {

  inherit systemPackages haskellPackages haskellUtilities;
  inherit static strip;

};

#------------------------------------------------#

in
##################################################
{

  inherit spiros;

}
##################################################