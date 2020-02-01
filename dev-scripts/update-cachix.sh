#!/usr/bin/env bash

# This is weird because for development I build using nix + stack rather than nix alone.
# see https://github.com/cachix/cachix/issues/52
# And of course, it will only work if you have my API key.

for f in shell.nix default.nix; do
  nix-store -qR --include-outputs $(nix-instantiate $f) | cachix push jefdaj
done
