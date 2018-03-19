#!/bin/bash
set -e

./install.sh '{}' 
# =
# nix-build shell.nix --show-trace --arg options '{}' --arg packageDotNix null
