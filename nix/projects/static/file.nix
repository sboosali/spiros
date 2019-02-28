##################################################
{ config

, cabal
, lib

, ...
}:

##################################################
''
--  -*- mode: cabal; buffer-read-only: t -*-  vim:set ro: 

--------------------------------------------------
-- Projects --------------------------------------
--------------------------------------------------

packages: ${cabal.list config.packages}

--------------------------------------------------

${lib.optionalString (config ? optional-packages) ''optional-packages: ${cabal.list config.optional-packages}''}

--------------------------------------------------
-- Statically-Linked Executables
--------------------------------------------------

static: True
shared: False

--------------------------------------------------

flags: +static

--------------------------------------------------

${cabal.extra-lib-dirs { inherit (config) libraries; stanza = true; }}

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
''