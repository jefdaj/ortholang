#!/usr/bin/env bash

# TODO launch this from inside nix-shell
# TODO and with root?

# stack build --pedantic --ghc-options="-Wno-orphans" --allow-different-user # \
stack build --ghc-options="-Wno-orphans" --allow-different-user \
  && ./.stack-work/install/*/*/*/bin/shortcut --test
