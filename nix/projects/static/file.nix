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
-- Static Linking --------------------------------
--------------------------------------------------

-- executable-static: True

static: True
shared: False

--------------------------------------------------

flags: +static

--------------------------------------------------

package *

  ----------------------------

  ${cabal.extra-lib-dirs { inherit (config) libraries; stanza = true; }}

  ----------------------------

  ghc-options:

    -fPIC

    ${if (config ? strip) then (if config.strip then "--enable-executable-stripping" else "--disable-executable-stripping") else ""}

  ----------------------------

  extra-lib-dirs:     ${cabal.path ~/.nix-profile/lib}
  extra-include-dirs: ${cabal.path ~/.nix-profile/include}
  extra-prog-path:    ${cabal.path ~/.nix-profile/bin}
  extra-prog-path:    ${cabal.path ~/.nix-profile/libexec}

  ----------------------------

-- ^ the <<< package * >>> stanza applies to all packages,
-- both local (internal / project) packages
-- and rempte (external / dependency) packages

--------------------------------------------------
-- Settings --------------------------------------
--------------------------------------------------

verbose: ${cabal.int config.verbose}
         -- <<< 1 >>> by default.

jobs: ${cabal.int config.jobs}
      -- <<< $ncpus >>> by default.

--------------------------------------------------

${lib.optionalString (config ? nix) ''nix: ${cabal.bool config.nix}''}

-- deterministic: {cabal.bool config.deterministic}
relocatable:   ${cabal.bool config.relocatable}

--------------------------------------------------

-- {field config "test" "test"}

${lib.optionalString (config ? test)  ''tests: ${cabal.bool config.test}''}

${lib.optionalString (config ? bench) ''benchmarks: ${cabal.bool config.bench}''}

${cabal.field { inherit config; cabalField = "documentation"; nixField = "docs"; }}

-- ${lib.optionalString (config ? docs)  ''documentation: ${cabal.bool config.docs}''}

--------------------------------------------------
-- Compiler --------------------------------------
--------------------------------------------------

${lib.optionalString (config ? compiler) ''with-compiler: ${cabal.path config.compiler}''}

--------------------------------------------------
-- Repositories ----------------------------------
-------------------------------------------------

${lib.optionalString (config ? lts) ''
  repository stackage-lts-${config.lts}
    url: https://www.stackage.org/lts-${config.lts}
''}

--------------------------------------------------
-- Notes -----------------------------------------
--------------------------------------------------
-- 
-- 
-- 
--------------------------------------------------
''