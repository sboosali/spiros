{ mkDerivation, base, bytestring, case-insensitive, containers
, cpuinfo, data-default-class, deepseq, directory, doctest
, exceptions, generic-deriving, hashable, mtl, optparse-applicative
, prettyprinter, process, safe, semigroups, show-prettyprint, split
, stdenv, stm, string-conv, template-haskell, text, time
, transformers, unordered-containers, vector, vinyl
}:
mkDerivation {
  pname = "spiros";
  version = "0.3.2";
  src = ../../spiros;
  configureFlags = [ "-fstatic" ];
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    base bytestring case-insensitive containers cpuinfo
    data-default-class deepseq directory exceptions generic-deriving
    hashable mtl prettyprinter process safe semigroups show-prettyprint
    split stm string-conv template-haskell text time transformers
    unordered-containers vector vinyl
  ];
  executableHaskellDepends = [ base optparse-applicative text ];
  testHaskellDepends = [ base doctest ];
  doCheck = false;
  homepage = "https://github.com/sboosali/spiros#readme";
  description = "Custom Prelude (sboo / sboosali)";
  license = stdenv.lib.licenses.asl20;
}
