##################################################
{ haskellPackages
}:

##################################################
rec {

  spiros      = haskellPackages.callPackage ./spiros.nix {
    # inherit spiros-base;
  };

  # spiros-base = haskellPackages.callPackage ./spiros-base.nix {
  # };

}
##################################################