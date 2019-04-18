#!/usr/bin/env bash

# remember to export TMPDIR=<some shared location> before testing on HPC clusters!
# script arguments will be passed to shortcut --test
# other possible tasty settings: https://hackage.haskell.org/package/tasty

export TASTY_QUICKCHECK_TESTS=1000
export TASTY_COLOR="always"
export TASTY_QUICKCHECK_SHOW_REPLAY=True
export TASTY_HIDE_SUCCESSES=False
[[ -z "$TMPDIR" ]] && export TMPDIR=/tmp
TEST_ARGS="--test $@ +RTS -IO -N -RTS"

# this does an incremental build of the haskell code for faster testing
echo "testing nix+stack build..."
cmd="(stack build --allow-different-user && ./.stack-work/install/*/*/*/bin/shortcut $TEST_ARGS) || exit"
nix-shell --command "$cmd" 2>&1 | tee stack-test.log

# this builds everything at once, which is simpler.
# the downside is it rebuilds the haskell code from scratch.
echo "testing nix build..."
(nix-build -j$(nproc) && ./result/bin/shortcut $TEST_ARGS) 2>&1 | tee nix-test.log
