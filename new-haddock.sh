#!/bin/sh

nix-shell --run "cabal new-haddock" "$@"

xdg-open ./dist-newstyle/build/x86_64-linux/ghc-8.2.2/spiros-0.2/doc/html/spiros/index.html  >/dev/null 2>&1
