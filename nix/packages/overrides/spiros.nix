##################################################
{ systemPackages
, haskellPackages
, haskellUtilities
}:

##################################################
let

#------------------------------------------------#

in
##################################################
oldDerivation: {

  # extraLibraries = oldDerivation.extraLibraries ++ (with static; [ gmp zlib ]);

  # extraLibraries = oldDerivation.extraLibraries ++ (with systemPackages; [ fltk ]);

}
##################################################