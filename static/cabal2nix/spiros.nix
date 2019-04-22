{ mkDerivation, base, bytestring, case-insensitive, containers
, cpuinfo, data-default-class, deepseq, directory, doctest
, exceptions, filepath, generic-deriving, hashable, mtl
, optparse-applicative, prettyprinter, process, safe, semigroups
, split, stdenv, stm, string-conv, template-haskell, text
, th-lift-instances, time, transformers, unix-compat
, unordered-containers
}:
mkDerivation {
  pname = "spiros";
  version = "0.4.4";
  src = ../../spiros;
  configureFlags = [ "-fexamples" "-fstatic" ];
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    base bytestring case-insensitive containers cpuinfo
    data-default-class deepseq directory exceptions filepath
    generic-deriving hashable mtl prettyprinter process safe semigroups
    split stm string-conv template-haskell text th-lift-instances time
    transformers unix-compat unordered-containers
  ];
  executableHaskellDepends = [ base optparse-applicative text ];
  testHaskellDepends = [ base doctest ];
  doCheck = false;
  homepage = "https://github.com/sboosali/spiros#readme";
  description = "Spiros Boosalis's Custom Prelude";
  license = stdenv.lib.licenses.asl20;
}
