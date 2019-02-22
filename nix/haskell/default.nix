##################################################
{ pkgs
, lib

, compiler ? null

           # ^ the haskell compiler. GHC 8.6.3 (by default).

, static ? null #TODO#

           # ^ if true, create statically-linkable libraries
           #   (i.e. « .a » files on Linux, « .dylib » files on OSX).
           #   if null, use the default behavior of « cabal ».

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

# For more information about the GHC (/ Cabal) options, see
# « https://github.com/NixOS/nixpkgs/blob/master/pkgs/development/haskell-modules/lib.nix ».

##################################################

assert (integer-simple -> (compiler != "ghcjs"));

#------------------------------------------------#

#TODO# enableRelocatedStaticLibs = true;

/*TODO sbooOverrides = sel: sup: 
  {

    mkDerivation
      = drv: sup.mkDerivation (drv //
         {
           jailbreak   = true; 
           doHaddock   = true;
           doCheck     = false;
           doBenchmark = false;
         });

  };
*/

##################################################
let
#------------------------------------------------#

haskellPackages =

  let

  cabalAttributes =

    {

      doCheck     = test;
      doBenchmark = bench;

      doHaddock   = docs;
      doCoverage  = cover;
      hyperlinkSource = true;

      dontStrip                 =  (! strip);
      enableDeadCodeElimination = dce;

   } // (lib.optionalAttrs (static != null)
          {
            enableStaticLibraries = static;
            
            enableSharedExecutables = (! static);
            enableLibraryProfiling  = (! static);
          }
   ) ;

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