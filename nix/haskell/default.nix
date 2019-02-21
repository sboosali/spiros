##################################################
{ pkgs

, compiler ? null

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

#------------------------------------------------#

##################################################
let
#------------------------------------------------#

haskellPackages =

  let

  cabalAttributes =

    {
      doCheck     = test;
      doBenchmark = bench;
    };

  hpkgs1 =

    if   null == compiler
    then pkgs.haskellPackages
    else

    if   integer-simple && (compiler != "ghcjs")
    then pkgs.haskell.packages.integer-simple.${compiler}

    else pkgs.haskell.packages.${compiler};

  hpkgs2 =

    hpkgs1.override {

        overrides = self: super: {

            mkDerivation = args:

                (super.mkDerivation (args // cabalAttributes));
        };
    };

  hpkgs3 = hpkgs2;

  # hpkgs3 =
  #   if   test
  #   then hpkgs2.override {
  #          overrides = self: super: {
  #            mkDerivation = args: super.mkDerivation (args // { enableLibraryProfiling = true; });
  #          };
  #        }
  #   else hpkgs2;

  in

  hpkgs3;

#------------------------------------------------#

haskellUtilities =

    pkgs.haskell.lib;

#------------------------------------------------#
in
##################################################
{

 inherit haskellPackages;
 inherit haskellUtilities;

}
##################################################