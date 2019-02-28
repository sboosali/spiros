##################################################
{ pkgs
, lib
}:

##################################################
let

cabal = import ../../cabal/lib.nix {

  inherit pkgs lib;

};

in
##################################################

cabal.mkCabalProjectFile {

  name   = "static";

  file   = ./file.nix;
  config = ./config.nix;

}

##################################################