#!/bin/bash
set -e
########################################

./environment.sh --pure --arg packageDotNix ./nix/spiros_only-library.nix --arg minimalDependencies true 

########################################
