##################################################
{ pkgs
, lib

, musl ? false

, ...
}:

##################################################
let
#------------------------------------------------#

ghc = pkgs.haskell.compiler.static.ghc863
   or pkgs.haskell.compiler.ghc864
   or pkgs.haskell.compiler.ghc863
   or pkgs.haskell.compiler.ghc862
   or pkgs.haskell.compiler.ghc861
   ;

#------------------------------------------------#
in
##################################################
rec {

  packages = [

    ../../../spiros

  ];

  verbose = 2;
  jobs    = 1;

  nix           = false;
  deterministic = true;
  relocatable   = true;
  strip         = true;

  test          = false;
  benchmark     = false;
  documentation = false;

  lts      = "13.9";
  # ^ Stackage LTS version.

  libraries = (with pkgs; [

    gmp
    libffi

  ]) ++ lib.optional musl (with pkgs; [

    musl

  ]);

  compiler = ''${ghc}/bin'';

  # ^ GHC executable path.
  # * should work with the stackage snapshot (i.e. « config.lts »).
  # * must be built with « -fPIC » (i.e. the « ghc » itself).

  # Old:
  #
  # compiler = 
  # compiler = ~/.nix-profile/bin/ghc-8.6.3;
  # compiler = /nix/store/8wbqsq3fnp8nkbpjp6kb0z66x6ynl9vz-ghc-8.6.3/bin/ghc;
  #

}
##################################################