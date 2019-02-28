##################################################
{ ...
}:

##################################################
rec {

  verbose = 2;
  jobs    = 4;

  nix           = false;
  deterministic = true;
  relocatable   = true;
  strip         = true;

  test          = false;
  benchmark     = false;
  documentation = false;

  lts      = "13.7";
  # ^ Stackage LTS version.

  compiler = ~/.nix-profile/bin/ghc-8.6.3;
  # ^ GHC executable path.
  # * should work with the stackage snapshot (i.e. « config.lts »).
  # * must be built with « -fPIC » (i.e. the « ghc » itself).

  libraries = { pkgs }: with pkgs; [

  ];

}
##################################################