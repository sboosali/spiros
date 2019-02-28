##################################################
{ pkgs

, ...
}:

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

  compiler = ~/.nix-profile/bin/ghc-8.6.3; #TODO#
  # ^ GHC executable path.
  # * should work with the stackage snapshot (i.e. « config.lts »).
  # * must be built with « -fPIC » (i.e. the « ghc » itself).

  libraries = with pkgs; [

  ];

}
##################################################