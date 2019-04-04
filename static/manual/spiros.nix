##################################################
# flags

{ doStrip
, ...
}:

#------------------------------------------------#
# « systemPackages »

{ lib

, gmp6, zlib, libffi
}:

#------------------------------------------------#
# « haskellPackages »

{ mkDerivation

, base, bytestring, case-insensitive, containers
, cpuinfo, data-default-class, deepseq, directory
, exceptions, generic-deriving, hashable, mtl, optparse-applicative
, prettyprinter, process, safe, semigroups, show-prettyprint, split
, stdenv, stm, string-conv, template-haskell, text, time
, transformers, unix-compat, unordered-containers, vector, vinyl

}:

##################################################

mkDerivation {

  pname   = "example-spiros";
  version = "0.3.2";
  src     = ../../spiros;

  configureFlags = [
    "-fstatic"
    "--ghc-option=-optl=-static"
    "--extra-lib-dirs=${gmp6}/lib"
    "--extra-lib-dirs=${zlib}/lib"
    "--extra-lib-dirs=${libffi}/lib"
  ] ++ lib.optionals (! doStrip) [
    "--disable-executable-stripping"
  ];

  isLibrary    = false;
  isExecutable = true;

  executableHaskellDepends    = [
    base bytestring case-insensitive containers cpuinfo
    data-default-class deepseq directory exceptions generic-deriving
    hashable mtl prettyprinter process safe semigroups show-prettyprint
    split stm string-conv template-haskell text time transformers
    unix-compat unordered-containers vector vinyl optparse-applicative 
  ];

  doCheck     = false;
  doBenchmark = false;

  enableSharedExecutables = false;
  enableSharedLibraries   = false;

  homepage    = "https://github.com/sboosali/spiros#readme";
  description = "Custom Prelude (sboo / sboosali)";
  license     = stdenv.lib.licenses.asl20;

}
##################################################