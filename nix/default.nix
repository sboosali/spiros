##################################################
arguments@

{ nixpkgs  ? <nixpkgs>
, overlays ? []
, config   ? {}

           # ^ (these options (above) affect only « pkgs » (below).)

, compiler ? "ghc863"

           # ^ the haskell compiler. GHC 8.6.3 (by default).

, musl     ? false

           # ^ if true, « musl » as the C Library (via « nixpkgs.pkgsMusl »).
           #   if false, « glibc » as the C Library (via « nixpkgs.pkgs »).

, integer-simple ? false

           # ^ if true, « integer-simple » as GHC's numeric library.
           #   if false, « gmp » as GHC's numeric library (the default).

, test     ? false
, bench    ? false
, docs     ? false

           # ^ build the « test:_ » and/or « benchmark:_ » components,
           #   and/or build documentation (i.e. Haddock).

, strip    ? true

           # ^ enable "executable stripping".

, ... }:

##################################################

assert (integer-simple -> (compiler != "ghcjs"));

##################################################
let
#------------------------------------------------#

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

haskell = import ./haskell
  {
    inherit pkgs;
    inherit compiler integer-simple;
    inherit test bench docs strip;
  };

inherit (haskell)
  haskellPackages
  haskellUtilities
  ;

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