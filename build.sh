#!/usr/bin/env bash

# remember to export TMPDIR=<some shared location> before testing on HPC clusters!
# script arguments will be passed to shortcut --test
# other possible tasty settings: https://hackage.haskell.org/package/tasty

export TASTY_QUICKCHECK_TESTS=1000
export TASTY_HIDE_SUCCESSES=True

# this builds everything at once, which is simpler.
# the downside is it rebuilds the haskell code from scratch.
# nix-build && ./result/bin/shortcut --test $@ # 2>&1 | tee test.log

# this does an incremental build of the haskell code for faster testing
export STACK_ROOT=$PWD/src/.stack-work
mkdir -p $STACK_ROOT
nix-shell --command '(cd src && stack build && ./.stack-work/install/*/*/*/bin/shortcut --test) || exit'
