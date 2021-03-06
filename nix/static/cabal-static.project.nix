##################################################
{ pkgs
}:

##################################################
let
#------------------------------------------------#

cabal = rec {

  bool = b:

      if b then "True" else "False";

  int  = builtins.toString;

  path = builtins.toString;

  list = xs:

    (builtins.toString (builtins.map path xs));

  # ^ space-separated items.

};

#------------------------------------------------#
in
##################################################
let
#------------------------------------------------#

config = {

  packages = [
    ../../spiros
  ];

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

};

#------------------------------------------------#

static = {

  gmp   = pkgs.gmp6.override { withStatic = true; };

  glibc = pkgs.glibc.static;

  zlib  = pkgs.zlib.static;

};

#------------------------------------------------#

# configureFlags-forStaticLinking = [

#           "--ghc-option=-optl=-static"

#           "--extra-lib-dirs=${static.gmp}/lib"
#           "--extra-lib-dirs=${static.zlib}/lib"

#         ] ++ pkgs.lib.optionals (! config.strip) [

#           "--disable-executable-stripping"

#         ];

#------------------------------------------------#

project-text = ''
--  -*- mode: cabal; buffer-read-only: t -*-  vim:set ro: 

--------------------------------------------------
-- Projects --------------------------------------
--------------------------------------------------

packages: ${cabal.list config.packages}

--------------------------------------------------
-- Statically-Linked Executables
--------------------------------------------------

static: True
shared: False

--------------------------------------------------

flags: +static

--------------------------------------------------

package *

  ----------------------------

  ghc-options:

    -fPIC

    -optl=-static
    -optl=-pthread

    -optl=-L${static.gmp}/lib
    -optl=-L${static.glibc}/lib
    -optl=-L${static.zlib}/lib

    ${if (! config.strip) then "--disable-executable-stripping" else "--enable-executable-stripping"}

  ----------------------------

  extra-lib-dirs:

    ${static.gmp}/lib
    ${static.glibc}/lib
    ${static.zlib}/lib

  ----------------------------

  extra-lib-dirs:     ${cabal.path ~/.nix-profile/lib}
  extra-include-dirs: ${cabal.path ~/.nix-profile/include}
  extra-prog-path:    ${cabal.path ~/.nix-profile/bin}
  extra-prog-path:    ${cabal.path ~/.nix-profile/libexec}

  ----------------------------

-- ^ the « package * » stanza applies to all packages,
-- both local (internal / project) packages
-- and rempte (external / dependency) packages

--------------------------------------------------
-- Settings --------------------------------------
--------------------------------------------------

-- nix: ${cabal.bool config.nix}

verbose: ${cabal.int config.verbose}
         -- « 1 » by default.

jobs: ${cabal.int config.jobs}
      -- « $ncpus » by default.

--------------------------------------------------

-- package *
--   extra-lib-dirs:     ${cabal.path ~/.nix-profile/lib}
--   extra-include-dirs: ${cabal.path ~/.nix-profile/include}
--   extra-prog-path:    ${cabal.path ~/.nix-profile/bin}
--   extra-prog-path:    ${cabal.path ~/.nix-profile/libexec}

--------------------------------------------------

deterministic: ${cabal.bool config.deterministic}
relocatable:   ${cabal.bool config.relocatable}

--------------------------------------------------

tests: ${cabal.bool config.test}

--------------------------------------------------

-- program-default-options
--   ld-options:

--------------------------------------------------
-- Compiler --------------------------------------
--------------------------------------------------

with-compiler: ${cabal.path config.compiler}

--------------------------------------------------
-- Repositories ----------------------------------
-------------------------------------------------

repository stackage-lts-${config.lts}
  url: https://www.stackage.org/lts-${config.lts}

--------------------------------------------------
-- Notes -----------------------------------------
--------------------------------------------------
-- 
-- 
-- 
--------------------------------------------------
'';

#------------------------------------------------#

project-file = pkgs.writeTextFile

    {
      name = "cabal-static.project";
      text = project-text;
    };

    # , executable  ? false  # run chmod +x ?
    # , destination ? ""     # relative path appended to $out eg "/bin/foo"
    # , checkPhase  ? ""     #  syntax checks, e.g. for scripts

#------------------------------------------------#
in
##################################################

project-file

##################################################