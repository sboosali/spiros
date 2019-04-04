##################################################
# flags

{ doStrip
, ...
}:

#------------------------------------------------#
# « systemPackages »

{ lib, haskellUtilities

, gmp6, zlib, libffi
}:

#------------------------------------------------#
# « haskellPackages »

haskellPackages@
{ mkDerivation, base, ... }:

##################################################
let                 # Imports...
#------------------------------------------------#

# the automatically-generated « derivation »:

spiros0 = haskellPackages.callPackage ./cabal2nix/spiros.nix {

  #TODO...
  #     put any project-packages (i.e. « packages: ... » in the « cabal.project »)
  #     or any vendored-packages (i.e. « optional-packages: ... » in the « cabal.project ») here.

};

#------------------------------------------------#
in
##################################################
let                 # Utilities...
#------------------------------------------------#

addExtraLibDirs = { subdir ? "/lib" }: directories: pkg:

  let

  flags = builtins.map extra-lib-dir directories;

  extra-lib-dir = dir:
    let
    dir' = dir + (builtins.toString subdir);
    in
    ''--extra-lib-dir=${dir'}'';

  in

  haskellUtilities.appendConfigureFlags pkg flags;

#------------------------------------------------#

addExtraLibDir = { subdir ? "/lib" }: directory:

  addExtraLibDirs { inherit subdir; } [ directory ];

#------------------------------------------------#
in
##################################################
let                 # Overrides...
#------------------------------------------------#

addStaticLibs = drv:

  addExtraLibDirs {} [ gmp6 zlib libffi ] drv;

#------------------------------------------------#

addStaticFlags = drv0:
  let

  drv1 = haskellUtilities.enableCabalFlag      (haskellUtilities.enableCabalFlag drv0 "examples") "static";
  drv2 = haskellUtilities.appendConfigureFlag  drv1 "--ghc-option=-optl=-static";
  drv3 = haskellUtilities.appendConfigureFlags drv2 (lib.optionals (! doStrip) [ "--disable-executable-stripping" ]);

  in
  drv3;

#------------------------------------------------#

/* Like « haskellUtilities.justStaticExecutables », but more aggressive.
 *
 * Enables all « executable » components, disabling all other components
 * (i.e. « library », « test-suite », « benchmark », « foreign-library » (?), and Haddocks).
 *
 * Enables « -static » artifacts,
 * disabling all « -shared » artifacts.
 * 
 * 
 */

onlyStaticExecutables = drv0:

  let
  override = attrs:
  {
    isExecutable = true;
    isLibrary    = false;

    doCheck     = false;
    doBenchmark = false;
    doHaddock   = false;

    enableSharedExecutables = false;
    enableSharedLibraries   = false;
    enableLibraryProfiling  = false;

    # executableHaskellDepends =
    #   (attrs.executableHaskellDepends or []) ++ (attrs.libraryHaskellDepends or []);

    postFixup = ''
    rm -rf $out/lib $out/nix-support $out/share/doc || true
    rmdir $out/share                                || true
    '';

    # ^ Keep « $out/share/man/1/ » and  « $out/share/{bash,zsh,fish}-completion" » (if they exist).

  };

  in

  haskellUtilities.overrideCabal drv0 override;

#------------------------------------------------#

toStaticSpiros = drv0:

  let
  drv1 = addStaticLibs                  drv0;
  drv2 = addStaticFlags                 drv1;
  drv3 = onlyStaticExecutables          drv2;
  drv4 = haskellUtilities.overrideCabal drv3 override;

  override = attrs:
  {
    pname = "example-spiros";
    # ^ just the « executable » name,
    #   not the full « .cabal » (/ « library ») name.
  };
  in

  drv4;

#------------------------------------------------#

# the manually-override « derivation »:

spiros1 = toStaticSpiros spiros0;

#------------------------------------------------#
in
##################################################

spiros1

##################################################