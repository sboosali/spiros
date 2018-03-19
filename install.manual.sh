#!/bin/bash
set -e

./install.sh '{}' --arg packageDotNix ./spiros.nix
# =
# nix-build shell.nix --show-trace --arg options '{}' --arg packageDotNix ./spiros.nix
