{ stdenv

, os   ? stdenv.buildPlatform.parsed.kernel.name
, arch ? stdenv.buildPlatform.parsed.cpu.name

, compilerName    ? "ghc"
, compilerVersion ? "8.2.2" #TODO

, doCheck     ? true
, doBenchmark ? false

, examples     ? true
, test-doctest ? true
, test-unit    ? true
#, test-static  ? false

, _config ? {
  platform = {
    inherit os arch;
  }
  impl = {
    name    = compilerName;
    version = compilerVersion;
  };
  flags = {
    inherit doCheck doBenchmark;
    inherit examples test-doctest test-unit test-static;
  };
};
# ^ "save" the configuration variables in a "reserved" variable to prevent possible shadowing

, systemPackages ? {}
  # most haskell packages don't require any particular foreign library or executable on the path.

, haskellPackages 
# , mkDerivation

}:
########################################
#NOTES

/*

The `config` (nix) argument represents a `List ConfVar` (haskell):

    -- package Cabal-2.2.0.0
    -- module Distribution.Types.GenericPackageDescription

    data ConfVar
      = OS   OS
      | Arch Arch
      | Impl CompilerFlavor VersionRange
      | Flag FlagName           

It has been inlined into kwargs (keyword arguments, i.e. `{os,arch,...}:`), so that `nix-*` commands (like `nix-shell`) can apply them more conveniently, e.g.:

  nix-shell --arg doctest true --argstr impl ghc-8.4

Thus, the (haskell) representation of these kwargs is like:

   (OS, Arch, CompilerFlavor, VersionRange, FlagAssignment)
   -- where
   type FlagAssignment = [(FlagName,Bool)]

`OS`, `Arch`, and `CompilerFlavor` are "pseudo-enums": each is a union of a "known set" (an enum) and an "unknown set" (a string). For example, "ghc" and "ghcjs" are "known compiler flavors", while "haste" and "frege" are "unknown compiler flavors" (from the perspective of `cabal`, as well as the `nixpkgs`). These "knowns" can be used to partially-or-completely evaluate different alternatives of a `.cabal` file. By setting the "confvars" (`ConfVar`, configuration variable), which are booleans or "pseudo-enumss", we can branch through any conditionals. 

Most `Cabal` fields within stanzas for "components" (i.e. a `library`, `executable`, `test-suite`, `benchmark`, or `foreign-library`) are gate-able by "conditions". These conditions are simple logical expressions, functions of the "configuration variables" mentioned above. Commonly, they're: for adding dependencies to components (via `build-depends` fields); or, for dropping a component itself (via the `buildable` field).


*/
  
/* 

Cabal has several "built-in" configuration flags:

  $ cabal configure ...
  --enable-benchmarks
  --enable-executable-profiling
  --enable-library-stripping
  --enable-relocatable
  --enable-coverage
  --enable-executable-stripping
  --enable-library-vanilla
  --enable-shared
  --enable-debug-info
  --enable-library-coverage
  --enable-optimisation
  --enable-split-objs
  --enable-deterministic
  --enable-library-for-ghci
  --enable-optimization
  --enable-tests
  --enable-executable-dynamic
  --enable-library-profiling
  --enable-profiling             

where the most commonly used are:

  --enable-tests
  --enable-benchmarks
  --enable-library-profiling

which `nixpkgs` calls (by convention, respectively):

  doCheck        :: Bool
  doBenchmark    :: Bool
  withProfiling  :: Bool

*/

/* 

Below is the provenance of the arguments which represent configuration variables. 

- doCheck
  - `$ cabal configure --enable-tests`

- doBenchmark
  - `$ cabal configure --enable-benchmarks`

- os
  - conditioning on `os(...)` in the `.cabal`

- arch
  - conditioning on `arch(...)` in the `.cabal`

- impl
  - conditioning on `impl(...)` in the `.cabal`

- <flag>
  - one for each `flag` stanza in the `.cabal`

For example, a conditional in some stanza:

  -- .cabal
  if !impl(ghcjs) && !flag(test-doctest)
     buildable: False

becomes this conditional (in the respective expression):

  # .nix
  buildable = !config.impl == "ghcjs" && !config."test-doctest"; 

(afaik, the naming `impl`, which comes cabal, is that compilers are haskell `impl`ementations.)

As another example, this conditional on the compiler's version range:

  -- .cabal
  if impl(ghc >= 8.2) 
     ...

becomes:

  # .nix
  if    config.impl.name == "ghc" 
     && lib.versionAtLeast config.impl.version "8.2"
  then ... else []


*/

  
/* e.g.

  > nixpkgs = import <nixpkgs> {}
  > :a nixpkgs.pkgs
  
  > stdenv.buildPlatform.parsed.kernel.name
  "linux"

  > stdenv.buildPlatform.parsed.cpu.name 
  "x86_64"

  > haskellPackages.ghc.haskellCompilerName 
  "ghc-8.2.2"

  # `haskellCompilerName` is correct, 
  # even when the package isn't buildable
  # TODO confirm. 

  > haskell.packages.ghcjs.ghc.haskellCompilerName 
  "ghcjs"

*/


/* e.g.

  > nixpkgs = import <nixpkgs> {}
  > :a nixpkgs.pkgs
  
  > haskell.compiler.ghc841.haskellCompilerName 
  "ghc-8.4.1"

  > haskell.compiler.integer-simple.ghc841.haskellCompilerName 
  "ghc-8.4.1"

  > haskell.compiler.ghcjs.haskellCompilerName 
  "ghcjs"

  > pkgs.haskell.compiler.ghcjs.version 
  "0.2.0"

*/


/*NOTES 

the `nix` flag names and defaults (above) come from their respective `cabal` flags (below):

  flag examples
    default: True
  
  flag test-doctest
    default: True
  
  flag test-unit
    default: True 
  
  flag test-static
    default: False

*/



/*NOTES

`os(...)`



*/


/*NOTES 

`lib.versionAtLeast` provides a non-lexicographic ordering that's aware of numbers within strings:

  > stdenv.lib.versionAtLeast "ghc-7.10" "ghc-8"
  false

  > stdenv.lib.versionAtLeast "ghc-8.2.1" "ghc-8"
  true

  > stdenv.lib.versionAtLeast "ghc-10.0.0" "ghc-8"
  true

However, it's not magically aware of the structure of versioned names:

  > stdenv.lib.versionAtLeast "ghcjs-8" "ghc-8"
  false

So we must either parse it ourselves, or use an explicit version field:

  > pkgs.haskell.compiler.ghc822.haskellCompilerName 
  "ghc-8.2.2"

  > pkgs.haskell.compiler.ghc822.version 
  "8.2.2"

  > pkgs.haskell.compiler.ghcjs.haskellCompilerName 
  "ghcjs"

  > pkgs.haskell.compiler.ghcjs.version 
  "0.2.0"

*/

/*NOTES

`spiros.nix` is automatically (re)generated by `cabal2nix`.

`spiros.cabal.nix` is manually written, to losslessly represent `spiros.cabal`. 

i.e...

- it explicitly requires all configuration variables that `cabal` supports, including `flags`:
* this file is a binary function, rather than unary;
* the first positional argument has default values for all its keyword arguments, consistent with the `.cabal` file. 

(btw, this `config` argument is a shallow record ("attr set"), and it can be named `config`, i.e. `{...}@config:`. Neither `config` nor `system` are currently (2018) the name of any package on hackage, fwiw.) 

- it correctly uses those flags within `nix`, just like `cabal` does:
* pruning/adding dependencies;
* enabling/disabling different components (lib, exe, test, bench); 
* etc. 

that is, `config` looks like:

  { system    ? ...
  # builtin cabal flags
  , _os       ? ...
  , _arch     ? ...
  , _compiler ? ...
  # custom cabal flags (from `flag` stanzas within the `.cabal`)
  , <flag>    ? ...
  , <flag>    ? ...
  ...
  }

which in typed pseudo-`nix` is:

  { system    :: String
  # `null` means what?
  , _os       :: Maybe String
  , _arch     :: Maybe String
  , _compiler :: Maybe String
  # 
  , <flag>    :: Bool
  , <flag>    :: Bool
  ...
  }

for example:

  # spiros.cabal.nix

  { system    ? builtins.currentSystem
  #
  , _os       ? null
  , _arch     ? null
  , _compiler ? null
  #
  , _examples ? true
  }:

  ... 

note, `currentSystem` is the `Platform`, with syntax like:

  <arch>-<os>

e.g.:

  nix-repl> builtins.currentSystem 
  "x86_64-linux"



*/

########################################
let

#NOTE
# Namespace the cabal "configuration variables", including 
# library-author-defined flags. By referencing them qualified,
# e.g. `config.os`, we prevent possible naming capture below
# (unless `config` itself is shadowed). 
config = {
 inherit os arch impl;
 inherit examples test-doctest test-unit test-static;
};

#NOTE
# Relatedly, the `.cabal` should not declare any user `flag`s named:
# - `doCheck`
# - `doBenchmark`

inherit (stdenv.lib)
 licenses;

inherit (systemPackages.lib)
 concatLists optionals versionAtLeast versionUnder;

inherit (haskellPackages)
 mkDerivation;

in
########################################
# setup
let
  
/* i.e.

custom-setup
  setup-depends: 
    <setupHaskellDepends>

*/
setupHaskellDepends = with haskellPackages; [
];

#TODO 
#, pkgconfigDepends ? [], libraryPkgconfigDepends ? [], executablePkgconfigDepends ? [], testPkgconfigDepends ? [], benchmarkPkgconfigDepends ? []

in
########################################
# library (/ libraries?)
let

/* i.e.

library

  build-depends:
    <libraryHaskellDepends>

  extra-libraries:
    <librarySystemDepends>

  build-tools:
    <libraryToolDepends>

*/

#TODO
# libraries = {
# };

library = {
  buildable    = true;
  dependencies = concatLists [
    libraryHaskellDepends
    librarySystemDepends
    libraryToolDepends
    libraryPkgconfigDepends
  ];
};

libraryHaskellDepends =
  with haskellPackages; 
  let config = _config; in

  concatLists [

    [ base bytestring containers data-default-class deepseq
      directory exceptions generic-deriving hashable mtl
      process safe safe-exceptions split stm string-conv
      template-haskell text time transformers unordered-containers
      vector vinyl
    ]

    (optionals
      ( (config.impl.name == "ghc")
        && 
        (versionUnder config.impl.version "8.0") 
        # `ghc-8.0.2`, a.k.a. `base-4.9.0.0`
      )
      [ semigroups
      ])

];

librarySystemDepends = with systemPackages; [
];

libraryToolDepends = with haskellPackages; [
];

libraryPkgconfigDepends = with systemPackages; [
];

in
########################################
# executable(s)
let

/* i.e.

executable <...>
  build-depends:
    <executableHaskellDepends>

*/

executables = {
};

executableHaskellDepends = with haskellPackages; [
];

executableSystemDepends = with systemPackages; [
];

executableToolDepends = with haskellPackages; [
];

executablePkgconfigDepends = with systemPackages; [
];

in
########################################
# test-suite(s)
let

/* i.e.

test-suite <...>
  build-depends:
    <testHaskellDepends>


  if    config.impl.name == "ghc" 
     && lib.versionAtLeast config.impl.version "8.2"

*/

testSuites = {

  "doctest" = {
    buildable = doCheck &&
      ( (config."test-doctest") && !(impl.name == "ghcjs")
      );
    dependencies = with haskellPackages; [
      base doctest
    ];
  };

  "unit" = {
    buildable    = doCheck && true;
    dependencies = with haskellPackages; [
      base tasty tasty-hunit
    ];
  };

};

testHaskellDepends = concatLists [
  (optionals testSuites."doctest".buildable
             testSuites."doctest".dependencies)
  (optionals testSuites."unit".buildable
             testSuites."unit".dependencies)
];

testSystemDepends = with systemPackages; [
];

testToolDepends = with haskellPackages; [
];

testPkgconfigDepends = with systemPackages; [
];

in
########################################
# benchmark(s)
let

/* i.e.

benchmark <...>
  build-depends:
    <benchmarkHaskellDepends>

*/

benchmarks = {
};

benchmarkHaskellDepends = with haskellPackages; [
];

benchmarkSystemDepends = with systemPackages; [
];

benchmarkToolDepends = with haskellPackages; [
];

benchmarkPkgconfigDepends = with systemPackages; [
];

in
########################################
let

components = {
  inherit library;
  inherit libraries;
  inherit testSuites
  inherit benchmarks;
};

in
########################################

/* 

for all keyword-arguments of `mkDerivation`, 
see `pkgs/development/haskell-modules/generic-builder.nix`.

- (head):
https://github.com/NixOS/nixpkgs/blob/master/pkgs/development/haskell-modules/generic-builder.nix

- (permalink circa March 2018):
https://github.com/NixOS/nixpkgs/blob/d3be925d3414375270a5dd3f97a68357b8ee87dd/pkgs/development/haskell-modules/generic-builder.nix 

*/

mkDerivation {

  pname   = "spiros";
  version = "0.2";
  src     = ./.;

  homepage    = "http://github.com/sboosali/spiros#readme";
  description = "my custom prelude";
  license     = licenses.bsd3;

  isLibrary    = true
  isExecutable = false;

  inherit setupHaskellDepends;

  inherit libraryHaskellDepends;
  inherit librarySystemDepends;
  inherit libraryToolDepends;
  inherit libraryPkgconfigDepends;

  inherit executableHaskellDepends;
  inherit executableSystemDepends;
  inherit executableToolDepends;
  inherit executablePkgconfigDepends;

  inherit testHaskellDepends;
  inherit testSystemDepends;
  inherit testToolDepends;
  inherit testPkgconfigDepends;

  inherit benchmarkHaskellDepends;
  inherit benchmarkSystemDepends;
  inherit benchmarkToolDepends;
  inherit benchmarkPkgconfigDepends;

}
########################################
