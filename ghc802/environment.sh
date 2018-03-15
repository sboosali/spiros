#!/bin/bash
nix-shell shell.nix  --argstr compiler ghc802  "$@"
