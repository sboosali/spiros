{ mkDerivation, base, bytestring, containers, data-default-class
, deepseq, directory, exceptions, generic-deriving
, hashable, mtl, prettyprinter, process, safe, safe-exceptions
, split, stdenv, stm, string-conv, template-haskell, text, time
, transformers, unordered-containers, vector, vinyl
}:
mkDerivation {
  pname = "spiros";
  version = "0.2";
  src = ./.;
  libraryHaskellDepends = [
    base bytestring containers data-default-class deepseq directory
    exceptions generic-deriving hashable mtl prettyprinter process safe
    safe-exceptions split stm string-conv template-haskell text time
    transformers unordered-containers vector vinyl
  ];
  homepage = "http://github.com/sboosali/spiros#readme";
  description = "my custom prelude";
  license = stdenv.lib.licenses.bsd3;
}
