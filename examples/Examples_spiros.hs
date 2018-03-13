{-# LANGUAGE
    CPP,
    NoImplicitPrelude
  #-}

{-# LANGUAGE
    
    PackageImports
  #-}

{-| Re-exports the examples.

== Usage

Quickstart (hackage):

@
# bash
nix-shell  --command 'ghci -package spiros -XNoImplicitPrelude -XOverloadedStrings; return'  -p 'haskell.packages.ghc822.ghcWithPackages (ps: with ps; [spiros])'
# ghci
:load Examples_spiros
:info Example.WarningValidation
@

Quickstart (github \/ local):

@
#
# bash
#
git clone <https://github.com/sboosali/spiros>
#
nix-shell -v --show-trace  --run 'ghci -package spiros -XNoImplicitPrelude -XOverloadedStrings'  -p '(haskell.packages.ghc822.override { overrides = self: super: { spiros = self.callCabal2nix ./spiros {}; }; }).ghcWithPackages (ps: with ps; [ spiros ])'
#
# ghci
#
:load Examples_spiros
:info Example.WarningValidation
example_validateNaturalRatio 2 3
...
#
@

Quickstart (script, also requires @bash@ and @nix@):

@
#
# run the above with a script (more convenient than manually, but less secure)
#
curl <https://raw.githubusercontent.com/sboosali/spiros/master/scripts/quickstart.sh> | sh
#
# requires `nix`; if you don't have it or don't want it, you can just obviously read thesimple script yourself and copy-paste it
#
curl <https://raw.githubusercontent.com/sboosali/spiros/master/scripts/quickstart.sh>
#
@

Details:

You can quickly experiment with any definitions (e.g. giving different inputs to the functions than the ones in the doctests) by [1] directly loading it into the interpreter, with the appropriate package and/or language settings:

@
$ ghci 
> :set -package spiros
> :set -XNoImplicitPrelude
> :set -XOverloadedStrings
> :load Examples_spiros
@

and [2] running the main function (if present) for a default test or for broad usage info, or calling whatever particular functions:

@
> main
...
> example_<...>
...
> :quit
$ 
@

assuming that [3] this package has been installed:

@

# nix
$ nix-shell -p 'haskell.packages.ghc822.ghcWithPackages (ps: with ps; [spiros])'

# stack
$ stack install spiros

# cabal
$ cabal install spiros
@

== Naming

Naming of @Examples_<PACKAGE>@: the lowercase and underscores of this formatting are weird, but it provides consistency for different package names. e.g. the examples of a package with a hyphenated name, like @validation-warning@, would be @Examples_validation_warning@. 

-}
module Examples_spiros
  ( module Example.WarningValidation 
  ) where

import Example.WarningValidation

----------------------------------------
