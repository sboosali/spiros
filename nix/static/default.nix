##################################################
{ nixpkgs  ? <nixpkgs>
, overlays ? []
, config   ? {}
           # ^ (these options (above) affect only « pkgs » (below).)

, pkgs     ? (import nixpkgs { inherit overlays config; }).pkgsMusl
           # ^ « musl » as C Library, not « glibc ».

, compiler ? "ghc863"
           # ^ the haskell compiler. GHC 8.6.3 (by default).

, strip    ? true
           # ^ enable "executable stripping".

}:

##################################################
let

inherit (pkgs) lib;

#------------------------------------------------#

haskellPackages =

    if   null == compiler
    then pkgs.haskellPackages
    else pkgs.haskell.packages.${compiler};

#------------------------------------------------#

haskellUtilities =

    pkgs.haskell.lib;

#------------------------------------------------#

in
##################################################
let

skeletor-static = import ./skeletor.nix {
  inherit pkgs haskellPackages haskellUtilities;
  inherit strip;
};

#------------------------------------------------#

cabal-project-string = import ./cabal.project.nix {
};

cabal-project-file = pkgs.writeTextFile

    {
      name = "cabal-static.project";
      text = cabal-project-string;
    };

    # , executable  ? false  # run chmod +x ?
    # , destination ? ""     # relative path appended to $out eg "/bin/foo"
    # , checkPhase  ? ""     #  syntax checks, e.g. for scripts

in
##################################################
{

  inherit skeletor-static;
  inherit cabal-project-file;

}
##################################################