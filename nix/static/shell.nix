##################################################
{ nixpkgs  ? <nixpkgs>
, overlays ? []
, config   ? {}
           # ^ (these options (above) affect only « pkgs » (below).)

, pkgs     ? (import nixpkgs { inherit overlays config; }).pkgsMusl
           # ^ « musl » as C Library, not « glibc ».

, compiler ? "ghc863"
           # ^ the haskell compiler. GHC 8.6.3 (by default).

, strip    ? true
           # ^ enable "executable stripping".

}:

##################################################
let

inherit (pkgs) lib;

#------------------------------------------------#

haskellPackages =

    if   null == compiler
    then pkgs.haskellPackages
    else pkgs.haskell.packages.${compiler};

#------------------------------------------------#

haskellUtilities =

    pkgs.haskell.lib;

#------------------------------------------------#

in
##################################################
let

environment = ;

in
##################################################
{

  inherit environment;

}
##################################################
# Notes ##########################################
##################################################

# Static Libraries for executable « example-spiros »:
#
# -lc
# -ldl
# -lffi
# -lgmp
# -lm
# -lpthread
# -lrt
# -lutil

##################################################