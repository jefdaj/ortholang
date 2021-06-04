#!/usr/bin/env bash

# This is weird because for development I build using nix + stack rather than nix alone.
# see https://github.com/cachix/cachix/issues/52
# And of course, it will only work if you have my API key.

nix-store -qR --include-outputs $(nix-instantiate release.nix) | cachix push jefdaj
