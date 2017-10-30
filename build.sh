#!/usr/bin/env bash

# other possible settings: https://hackage.haskell.org/package/tasty
export TASTY_QUICKCHECK_TESTS=1000
export TASTY_HIDE_SUCCESSES=True

# Only run certain tests; see https://github.com/feuerbach/tasty#patterns
# export TASTY_PATTERN=testpatternhere

# this builds everything at once, which is simpler.
# the downside is it rebuilds the haskell code each time, which is slow
# nix-build && ./result/bin/shortcut --test

# this does an incremental build of the haskell code for faster testing
nix-shell --command '(cd src && stack build && ./.stack-work/install/*/*/*/bin/shortcut --test) || exit'
