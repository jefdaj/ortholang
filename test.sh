#!/usr/bin/env bash

# TODO launch this from inside nix-shell
# TODO and with root?
# TODO stack setup the first time
# TODO set PATH for stack

# stack build --pedantic --ghc-options="-Wno-orphans" --allow-different-user # \
cd src
stack build --pedantic --ghc-options="-Wno-orphans" \
  && ./.stack-work/install/*/*/*/bin/shortcut --test
