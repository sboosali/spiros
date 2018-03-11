#!/bin/bash
NIX_SHELL_FILE=shell.nix
NIX_SHELL_OPTIONS=() # (--arg doBenchmark true)
nix-shell "$NIX_SHELL_FILE" "${NIX_SHELL_OPTIONS[@]}" "$@"
