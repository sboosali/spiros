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

args@
{ mkDerivation
, base, bytestring, case-insensitive, containers
, cpuinfo, data-default-class, deepseq, directory, doctest
, exceptions, filepath, generic-deriving, hashable, mtl
, optparse-applicative, prettyprinter, process, safe, semigroups
, show-prettyprint, split, stdenv, stm, string-conv
, template-haskell, text, time, transformers, unix-compat
, unordered-containers, vector, vinyl
}:

#TODO args@{ mkDerivation, base, ... }:

##################################################
let
#------------------------------------------------#

spiros0 = import ./cabal2nix/spiros.nix;

#------------------------------------------------#
in
##################################################
let
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
let
#------------------------------------------------#

addStaticLibs = pkg:

  addExtraLibDirs {} [ gmp6 zlib libffi ] pkg;

#------------------------------------------------#

addStaticFlags = pkg0:

  let
  pkg1 = haskellUtilities.appendConfigureFlags pkg0 (lib.optionals (! doStrip) [ "--disable-executable-stripping" ]);
  pkg2 = haskellUtilities.appendConfigureFlag  pkg1 "--ghc-option=-optl=-static";
  pkg3 = haskellUtilities.enableCabalFlag      pkg2 "static";
  in

  pkg3;

#------------------------------------------------#

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
    enableLibraryProfiling  = false;

    executableHaskellDepends =
      (attrs.executableHaskellDepends or []) ++ (attrs.libraryHaskellDepends or []);

    postFixup = "rm -rf $out/lib $out/nix-support $out/share/doc";
  };

  in

  haskellUtilities.overrideCabal drv0 override;

#------------------------------------------------#

toStaticSpiros = pkg0:

  let
  pkg1 = addStaticLibs  pkg0;
  pkg2 = addStaticFlags pkg1;
  pkg3 = onlyStaticExecutables pkg2;
  in

  pkg3;

#------------------------------------------------#

spiros1 = toStaticSpiros spiros0 args;

#------------------------------------------------#
in
##################################################

spiros1

##################################################