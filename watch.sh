#!/bin/bash

GHCID_FILE=./ghcid.txt

echo '...' > "$GHCID_FILE"
# emacsclient "$GHCID_FILE" &

COMMAND='nix-shell --run "cabal new-repl spiros"'
ghcid -o "$GHCID_FILE" --command "$COMMAND"

the Validation Applicative, which collects all (not just the first) errors, and also collects any warnings (whether a success or failure).