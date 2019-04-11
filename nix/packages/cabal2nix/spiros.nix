#  -*- mode: nix; buffer-read-only: t; -*-  
{ mkDerivation, base, bytestring, case-insensitive, containers
, cpuinfo, data-default-class, deepseq, directory, doctest
, exceptions, filepath, generic-deriving, hashable, mtl
, prettyprinter, process, safe, semigroups, split, stdenv, stm
, string-conv, template-haskell, text, th-lift-instances, time
, transformers, unix-compat, unordered-containers, vector
}:
mkDerivation {
  pname = "spiros";
  version = "0.4.0";
  src = /home/sboo/haskell/spiros/spiros;
  configureFlags = [ "-fstatic" ];
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    base bytestring case-insensitive containers cpuinfo
    data-default-class deepseq directory exceptions filepath
    generic-deriving hashable mtl prettyprinter process safe semigroups
    split stm string-conv template-haskell text th-lift-instances time
    transformers unix-compat unordered-containers vector
  ];
  testHaskellDepends = [ base doctest ];
  homepage = "https://github.com/sboosali/spiros#readme";
  description = "Spiros Boosalis's Custom Prelude";
  license = stdenv.lib.licenses.asl20;
}
