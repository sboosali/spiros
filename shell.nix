{ nixpkgs ? import <nixpkgs> {}

, spiros_nix ? ./nix/spiros.nix # ./.

, compiler ? "default"
/* =
"ghc7103"
"ghc802"
"ghc822"
"ghc841"
"ghcHEAD"
"ghc7103Binary"
"ghc821Binary"
"ghcjs"
"ghcjsHEAD"
"integer-simple"
*/

, integer-simple ? false
/* =
false
true
*/

, withHoogle  ? false 
#, withLLVM    ? false

, isProfiled    ? false
, isTested      ? false
, isBenchmarked ? false
, isDocumented  ? false
, isHyperlinked ? true
, isDwarf       ? false

, whichObjectLibrary ? "default"
/* = 
"static" 
"shared"
*/

, whichLinker ? "default"
/* = 
"gold" 
*/

, development   ? true
}:

/* Usage:

nix-shell --argstr compiler ...
ghc7103
ghc802
ghc822
ghc841
ghcHEAD
ghc7103Binary
ghc821Binary
ghcjs
ghcjsHEAD       
integer-simple



  nix-shell
  cabal configure 
  cabal build
  cabal run

*/

########################################
let

### "IMPORTS"

inherit (nixpkgs) pkgs;
inherit (pkgs)    stdenv;
# "import" utilities
inherit (pkgs)       fetchFromGitHub;
inherit (stdenv.lib) optionalAttrs;

lib = import "${nixpkgs.path}/pkgs/development/haskell-modules/lib.nix" { pkgs = nixpkgs; };
haskell = pkgs.haskell.lib; 

/* attrNames pkgs.haskell.lib;
=
addBuildDepend             :: ? -> ?
addBuildDepends            :: ? -> ?
addBuildTool               :: ? -> ?
addBuildTools              :: ? -> ?
addExtraLibraries          :: ? -> ?
addExtraLibrary            :: ? -> ?
addPkgconfigDepend         :: ? -> ?
addPkgconfigDepends        :: ? -> ?
addSetupDepend             :: ? -> ?
addSetupDepends            :: ? -> ?
appendConfigureFlag        :: ? -> ?
appendPatch                :: ? -> ?
appendPatches              :: ? -> ?
buildFromSdist             :: ? -> ?
buildStackProject          :: ? -> ?
buildStrictly              :: ? -> ?
checkUnusedPackages        :: ? -> ?
controlPhases              :: ? -> ?
disableCabalFlag           :: ? -> ?
disableDeadCodeElimination :: ? -> ?
disableHardening           :: ? -> ?
disableLibraryProfiling    :: ? -> ?
disableSharedExecutables   :: ? -> ?
disableSharedLibraries     :: ? -> ?
disableStaticLibraries     :: ? -> ?
doBenchmark                :: ? -> ?
doCheck                    :: ? -> ?
doCoverage                 :: ? -> ?
doDistribute               :: ? -> ?
doHaddock                  :: ? -> ?
doHyperlinkSource          :: ? -> ?
doJailbreak                :: ? -> ?
doStrip                    :: ? -> ?
dontBenchmark              :: ? -> ?
dontCheck                  :: ? -> ?
dontCoverage               :: ? -> ?
dontDistribute             :: ? -> ?
dontHaddock                :: ? -> ?
dontHyperlinkSource        :: ? -> ?
dontJailbreak              :: ? -> ?
dontStrip                  :: ? -> ?
enableCabalFlag            :: ? -> ?
enableDWARFDebugging       :: ? -> ?
enableDeadCodeElimination  :: ? -> ?
enableLibraryProfiling     :: ? -> ?
enableSharedExecutables    :: ? -> ?
enableSharedLibraries      :: ? -> ?
enableStaticLibraries      :: ? -> ?
extractBuildInputs         :: ? -> ?
failOnAllWarnings          :: ? -> ?
getHaskellBuildInputs      :: ? -> ?
ghcInfo                    :: ? -> ?
justStaticExecutables      :: ? -> ?
linkWithGold               :: ? -> ?
makePackageSet             :: ? -> ?
markBroken                 :: ? -> ?
markBrokenVersion          :: ? -> ?
overrideCabal              :: ? -> ?
overrideSrc                :: ? -> ?
packageSourceOverrides     :: ? -> ?
removeConfigureFlag        :: ? -> ?
sdistTarball               :: ? -> ?
shellAware                 :: ? -> ?
triggerRebuild             :: ? -> ?

*/

in
########################################
let

### UTILITIES

skipTests       = haskell.dontCheck; 
jailbreak       = haskell.doJailbreak;
dropUpperBounds = haskell.doJailbreak;

#:: String -> Path -> 
execCabal2nix = options: src:
  nixpkgs.runCommand "cabal2nix" {
    buildCommand = ''
      cabal2nix ${options} file://"${src}" >"$out"
    '';
    buildInputs = with nixpkgs; [
      cabal2nix
    ];
  } "";

execCabal2nixSubpath = subpath: src:
  nixpkgs.runCommand "cabal2nix" {
    buildCommand = ''
      cabal2nix --subpath "${subpath}" file://"${src}" >"$out"
    '';
    buildInputs = with nixpkgs; [
      cabal2nix
    ];
  } "";

in
########################################
let

### SOURCE OVERRIDES

# "megarepos" which have multiple packages as subdirectories.
repositories = {

};

# 
sources = {
};

in
########################################
let

# nix-shell --show-trace -p "(haskell.packages.${COMPILER}.override { overrides = self: super: { spiros = haskell.lib.dontCheck (haskell.lib.dontHaddock (self.callCabal2nix ''spiros'' ./. {})); }; }).ghcWithPackages (self: with self; [ spiros ])"

####################

customDerivationOptions = 
    { enableLibraryProfiling = isProfiled; 
      doCheck                = isTested; 
      doBenchmark            = isBenchmarked; 
      doHaddock              = isDocumented;
      doHyperlinkSource      = isDocumented && isHyperlinked;
      enableDWARFDebugging   = isDwarf;
    } //
    ( if   (whichObjectLibrary == "shared") 
      then { enableSharedLibraries  = true; 
           }
      else 
      if   (whichObjectLibrary == "static")
      then { enableStaticLibraries  = true; 
           }
      else
      if   (whichObjectLibrary == "both") # TODO
      then { enableSharedLibraries  = true;
             enableStaticLibraries  = true; 
           }
      else 
      if   (whichObjectLibrary == "default")
      then {}
      else {} # TODO
    ) // 
    optionalAttrs (whichLinker == "gold") 
      { linkWithGold = true;
      }
 ;

####################

hooglePackagesOverride = self: super:
  {
    ghcWithPackages = self.ghc.withPackages;

    ghc = super.ghc //
      { withPackages = super.ghc.withHoogle; 
      };
  };

####################

customMkDerivation = self: super: args:
  super.mkDerivation
    (args // customDerivationOptions);

####################

# llvmPackagesOverride = self: super:
#   {
#     ghcWithPackages = self.ghc.withPackages;

#     ghc = super.ghc //
#       { withPackages = super.ghc.llvmPackages; #TODO
#       };
#   };

### COMPILERS

haskellPackagesWithCompiler1 = 
  if   compiler == "default" # `integer-simple` is ignored
  then pkgs.haskellPackages
  else 
  if   integer-simple
  then pkgs.haskell.packages.integer-simple.${compiler}
  else pkgs.haskell.packages.${compiler};

haskellPackagesWithCustomPackages2 =
  if   withHoogle
  then haskellPackagesWithCompiler1.override {
         overrides = hooglePackagesOverride;
       }
  else haskellPackagesWithCompiler1;

haskellPackagesWithCustomDerivation3 = 
  haskellPackagesWithCustomPackages2.override {
    overrides = self: super: {
      mkDerivation = customMkDerivation self super;
    };
  };

# the last referenced, below
# TODO scoping
customizedHaskellPackages = haskellPackagesWithCustomDerivation3;

/*
nix-repl> haskell.packages._

haskell.packages.ghc7103
haskell.packages.ghc821Binary
haskell.packages.ghcHEAD
haskell.packages.integer-simple
haskell.packages.ghc7103Binary
haskell.packages.ghc822
haskell.packages.ghcjs
haskell.packages.ghc802
haskell.packages.ghc841
haskell.packages.ghcjsHEAD       

*/

in
########################################
### Haskell Dependencies...
let

/*

NOTES

* `local` / `github`: 
   They call `import` directly, thus those directories require a `default.nix`
* `cabal2nix` / `hackage` / `github2nix`: 
   They call `cabal2nix`, which generates the `default.nix`, so they don't require the given directory to be a valid `nix` package. 

TYPES
(in pseudo-typed-nix)

type Dependencies = { (Derivation | _) }

nix  : Path -> Dependencies -> Derivation
nix_ : Path ->              -> Derivation

hackage  : String/Name -> String/Version -> Dependencies -> Derivation
hackage_ : String/Name -> String/Version ->              -> Derivation                       

...

*/
myHaskellOverlaysWith = pkgs: self: super: let
#myHaskellOverlaysWith = pkgs: self: super: let

 nix        = path:
              self.callPackage path; 

 local      = path:
              self.callPackage path; 

 github     = o:
              self.callPackage (pkgs.fetchFromGitHub o); 

             # o ::
             #      { owner           :: String
             #        repo            :: String
             #        rev             :: String
             #        fetchSubmodules :: Bool
             #        sha256          :: String
             #      } 

 cabal2nix  = name: source: 
              self.callCabal2nix name source;

 hackage    = name: version:
              self.callHackage name version;

 github2nix = o:
              cabal2nix o.repo (pkgs.fetchFromGitHub o); 

 # override the package without overriding any dependencies
 nix_        = path:           nix        path         {};
 local_      = path:           local      path         {};
 github_     = o:              github     o            {};
 cabal2nix_  = name: source:   cabal2nix  name source  {};
 hackage_    = name: version:  hackage    name version {};
 github2nix_ = o:              github2nix o            {};

 #
 haskell = pkgs.haskell.lib; 
 dependsOn = package: dependencies: 
  haskell.addBuildDepends package dependencies;

 in

 let 
 reflex_dom = (import repositories.reflex-dom) self pkgs;
 in

 {
   ########################################
   # Add Haskell Packages Below           #
   ######################################## 

  # protolude = hackage_ "protolude" "0.2.1";

  # vinyl = skipTests super.vinyl; 

#
# test-suite doctests
#   build-depends:    base >= 4.7 && <= 5, lens, vinyl, doctest >= 0.8, singletons >= 0.10
#
# Configuring singletons-2.3.1...
# Setup: Encountered missing dependencies:
# base >=4.10 && <5
# builder for ‘/nix/store/qiaqn6ni9c8c6w8drlkx9iyf1djyx6r7-singletons-2.3.1.drv’ failed with exit code 1
# building path(s) ‘/nix/store/gi2hv1j8pbb5cn55h9lgx11g4ajryys1-protolude-0.2.1-doc’, ‘/nix/store/rz0rwxdl4n5cva0gdbs24zw8zc2p4iqk-protolude-0.2.1’
# cannot build derivation ‘/nix/store/1a61yj01hgv3f35324p0gfj8gn1327jy-vinyl-0.7.0.drv’: 1 dependencies couldn't be built
# killing process 14726
# cannot build derivation ‘/nix/store/p1m3pi9y22gpmar7bs0wb0lpvkkf37k8-ghc-8.0.2-with-packages.drv’: 1

 };

in
########################################
let

### OTHER OVERRIDES
 
modifiedHaskellPackages = customizedHaskellPackages.override {
#  overrides = self: super: {
  overrides = self: super:
    myHaskellOverlaysWith pkgs self super // {
  };
};

in
########################################
let

### DERIVATION / ENVIRONMENT

# theNixFile = ./.;

installationDerivation = modifiedHaskellPackages.callPackage
 spiros_nix {};

# development environment
# for `nix-shell --pure`
developmentDerivation = (haskell.addBuildDepends installationDerivation
 developmentPackages);

developmentPackages
  = developmentHaskellPackages
 # ++ developmentEmacsPackages 
 ++ developmentSystemPackages;

developmentSystemPackages = with pkgs; [
 #   
 cabal-install
 # 
 coreutils
 inotify-tools
 #   
 emacs
 git
 # 
];

developmentHaskellPackages = with modifiedHaskellPackages; [
 #   
 # ghcid
 # ghc-mod
 # 
 # 
 hoogle
 # 
 hasktags
 hlint
 # 
 present
 stylish-haskell
 hindent
 #   
];

 # developmentEmacsHaskellPackages = with Packages; [
 #    dante
 #  ];

environment = haskell.shellAware developmentDerivation;
   # if pkgs.lib.inNixShell then drv.env else drv;

in
########################################

environment

########################################
/*

*/
