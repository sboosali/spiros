#!/bin/sh

nix-shell --run "cabal haddock" "$@"

xdg-open ./dist/doc/html/spiros/index.html >/dev/null 2>&1

