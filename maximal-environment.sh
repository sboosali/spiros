#!/bin/bash
nix-shell shell.nix  --arg withHoogle true  --arg isProfiled true  --arg isTested true  --arg isBenchmarked true  --arg isDocumented true  --arg isHyperlinked true  --arg isDwarf true  --argstr whichLinker gold  --argstr whichObjectLibrary both  "$@"
