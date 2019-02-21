#  -*- mode: nix; buffer-read-only: t; -*-  
{ mkDerivation, base, bytestring, case-insensitive, containers
, cpuinfo, data-default-class, deepseq, directory, exceptions
, generic-deriving, hashable, mtl, prettyprinter, process, safe
, semigroups, show-prettyprint, split, stdenv, stm, string-conv
, template-haskell, text, time, transformers, unordered-containers
, vector, vinyl
}:
mkDerivation {
  pname = "spiros";
  version = "0.3.1";
  src = /home/sboo/haskell/spiros/spiros;
  libraryHaskellDepends = [
    base bytestring case-insensitive containers cpuinfo
    data-default-class deepseq directory exceptions generic-deriving
    hashable mtl prettyprinter process safe semigroups show-prettyprint
    split stm string-conv template-haskell text time transformers
    unordered-containers vector vinyl
  ];
  homepage = "http://github.com/sboosali/spiros#readme";
  description = "Custom Prelude (sboo / sboosali)";
  license = stdenv.lib.licenses.bsd3;
}
