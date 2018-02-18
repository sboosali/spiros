{ mkDerivation, base, bytestring, clock, containers
, data-default-class, deepseq, directory, hashable, mtl
, prettyprinter, process, safe, safe-exceptions, split, stdenv, stm
, string-conv, text, time, transformers, unordered-containers
, vector, vinyl
}:
mkDerivation {
  pname = "spiros";
  version = "0.0.1";
  src = ./.;
  libraryHaskellDepends = [
    base bytestring clock containers data-default-class deepseq
    directory hashable mtl prettyprinter process safe safe-exceptions
    split stm string-conv text time transformers unordered-containers
    vector vinyl
  ];
  homepage = "http://github.com/sboosali/spiros#readme";
  description = "my custom prelude";
  license = stdenv.lib.licenses.bsd3;
}
