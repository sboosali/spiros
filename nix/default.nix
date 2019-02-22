##################################################
arguments@

{ nixpkgs  ? (import ./nixpkgs)
, overlays ? []
, config   ? {}

           # ^ (these options (above) affect only « pkgs » (below).)

, static   ? false

           # ^ create statically-linked executables,
           #   and statically-linkable libraries.
           #   (or try to have as few dynamic library dependencies as possible).
           #   « static = true » implies « musl = true » (among other things).

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

           # ^ build the « test:_ » and/or « benchmark:_ » components.

, docs     ? false
, cover    ? false

           # ^ generate documentation (i.e. Haddocks) and/or a coverage report.

, dce      ? false

           # ^ « dce » abbreviates "Dead Code Elimination".

, strip    ? true

           # ^ enable "executable stripping".

, ... }:

##################################################

assert (integer-simple -> (compiler != "ghcjs"));

##################################################
let
#------------------------------------------------#

config = {

  musl           = static || musl;
  integer-simple = static || integer-simple;

  compiler-flavor = "ghc"; #TODO#

};

#------------------------------------------------#
in
##################################################
let
#------------------------------------------------#

inherit (config)
  musl
  integer-simple
  
  ;

#------------------------------------------------#

pkgs =

  let
  nixpkgs' = import nixpkgs { inherit overlays config; };
  in

  if   musl
  then nixpkgs'.pkgs
  else nixpkgs'.pkgsCross.musl64
# else nixpkgs'.pkgsMusl
  ;

#------------------------------------------------#

inherit (pkgs) lib;

#------------------------------------------------#

systemPackages = pkgs;

#------------------------------------------------#

haskell = import ./haskell
  {
    inherit pkgs lib;
    inherit static compiler integer-simple;
    inherit test bench docs cover strip dce;
  };

inherit (haskell)
  haskellPackages
  haskellUtilities
  ;

#------------------------------------------------#
in
##################################################
let
#------------------------------------------------#

packages = import ./packages {

  inherit systemPackages haskellPackages haskellUtilities;

  inherit strip;

};

#------------------------------------------------#

cabal = import ./cabal {

  inherit pkgs;

};

#------------------------------------------------#
in
##################################################
{

 inherit packages;
 inherit cabal;

}
##################################################