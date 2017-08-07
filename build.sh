#!/usr/bin/env bash

# other possible settings: https://hackage.haskell.org/package/tasty
export TASTY_QUICKCHECK_TESTS=1000
export TASTY_HIDE_SUCCESSES=True

# this builds everything at once, which is simpler.
# minor downside though: it rebuilds the haskell package each time
nix-build && ./result/bin/shortcut --test

# this does incremental builds of the haskell package,
# but has to be run inside nix-shell for the other dependencies
# cd src && stack build && ./.stack-work/install/*/*/*/bin/shortcut --test
