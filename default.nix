{ mkDerivation, base, bytestring, containers, data-default-class
, deepseq, directory, hashable, mtl, process, safe, semigroups
, split, stdenv, stm, text, time, transformers
, unordered-containers, vector, vinyl, wl-pprint-text
}:
mkDerivation {
  pname = "spiros";
  version = "0.0.0";
  src = ./.;
  libraryHaskellDepends = [
    base bytestring containers data-default-class deepseq directory
    hashable mtl process safe semigroups split stm text time
    transformers unordered-containers vector vinyl wl-pprint-text
  ];
  homepage = "http://github.com/sboosali/spiros#readme";
  description = "my custom prelude";
  license = stdenv.lib.licenses.bsd3;
}
