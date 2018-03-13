{-# LANGUAGE
    CPP,
    NoImplicitPrelude
  #-}

{-# LANGUAGE
    
    PackageImports
  #-}

{-| Re-exports the examples.

== Usage

=== Quickstart (already installed):

@

ghci -package spiros -XNoImplicitPrelude -XOverloadedStrings

:m +Prelude.Spiros +Examples_spiros
:info Example.WarningValidation

@

=== Quickstart (install from hackage)

nix-shell --show-trace  -p '(haskell.packages.ghc822.override { overrides = self: super: { spiros = haskell.lib.dontCheck (haskell.lib.dontHaddock (self.callCabal2nix "spiros" ./spiros {})); }; }).ghcWithPackages (self: with self; [ spiros ])'  --command "echo -e '\n:m +Prelude.Spiros Examples_spiros \nexample_validateNaturalRatio 2 3\n' ; ghci -package spiros -XNoImplicitPrelude -XOverloadedStrings "

=== Quickstart (install from github \/ locally):

# run a script to clone the latest version of this package, and to provision the environment via nix
#
git clone <https://github.com/sboosali/spiros>
#
nix-shell --show-trace  -p '(haskell.packages.ghc822.override { overrides = self: super: { spiros = haskell.lib.dontCheck (haskell.lib.dontHaddock (self.callCabal2nix "spiros" ./spiros {})); }; }).ghcWithPackages (self: with self; [ spiros ])'  --command "echo -e '\n:m +Prelude.Spiros Examples_spiros \nexample_validateNaturalRatio 2 3\n' ; ghci -package spiros -XNoImplicitPrelude -XOverloadedStrings "
#

=== Quickstart (script, also requires @bash@ and @nix@):

@
#
# run a script to provision the environment via nix
#
curl <https://raw.githubusercontent.com/sboosali/spiros/master/scripts/quickstart.sh> | sh
#
# (more convenient than manually, but obviously less secure)
#
# this requires `nix`; if you don't have it or don't want it, you can just obviously read thesimple script yourself and copy-paste-edit it
#
curl <https://raw.githubusercontent.com/sboosali/spiros/master/scripts/quickstart.sh>
#
@

=== Details:

You can quickly experiment with any definitions (e.g. giving different inputs to the functions than the ones in the doctests) by [1] directly loading it into the interpreter, with the appropriate package and/or language settings:

@
$ ghci 
> :set -package spiros
> :set -XNoImplicitPrelude
> :set -XOverloadedStrings
> :m +Prelude.Spiros +Examples_spiros
@

and [2] running the main function (if present) for a default test or for broad usage info, or calling whatever particular functions you want:

@
> main
...
> example_<x>
...
> example_<y>
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
