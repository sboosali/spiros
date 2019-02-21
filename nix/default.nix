##################################################
arguments@

{ nixpkgs  ? <nixpkgs>
, overlays ? []
, config   ? {}

           # ^ (these options (above) affect only « pkgs » (below).)

, musl     ? false

           # ^ if true, « musl » as the C Library (via « nixpkgs.pkgsMusl »).
           #   if false, « glibc » as the C Library (via « nixpkgs.pkgs »).

, compiler ? "ghc863"

           # ^ the haskell compiler. GHC 8.6.3 (by default).

, test     ? false
, bench    ? false
, docs     ? false

           # ^ build the « test:_ » and/or « benchmark:_ » components,
           #   and/or build documentation (i.e. Haddock).

, strip    ? true

           # ^ enable "executable stripping".

, ... }:

##################################################
let

pkgs =

  let
  nixpkgs' = import nixpkgs { inherit overlays config; };
  in

  if   musl
  then nixpkgs'.pkgs
  else nixpkgs'.pkgsMusl
  ;

#------------------------------------------------#

inherit (pkgs) lib;

#------------------------------------------------#

systemPackages = pkgs;

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

packages = import ./packages {

  inherit systemPackages haskellPackages haskellUtilities;

  inherit strip;

};

#------------------------------------------------#

in
##################################################
{

 inherit packages;

}
##################################################