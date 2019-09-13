#!/usr/bin/env bash

# remember to export TMPDIR=<some shared location> before testing on HPC clusters!
# script arguments will be passed to shortcut --test
# other possible tasty settings: https://hackage.haskell.org/package/tasty

export TASTY_QUICKCHECK_TESTS=1000
export TASTY_COLOR="always"
export TASTY_QUICKCHECK_SHOW_REPLAY=True
export TASTY_HIDE_SUCCESSES=True
[[ -z "$TMPDIR" ]] && export TMPDIR=/tmp

export NIX_CURL_FLAGS=-sS

export NIX_ARGS="-j$(nproc) --quiet"
export STACK_ARGS="--allow-different-user"
export TEST_ARGS="+RTS -IO -N -RTS --test $@"

# have to build dependencies either way
# this will make a series of result-N links, and those are saved by .travis.yml
echo "building dependencies..."
nix-build $NIX_ARGS dependencies.nix

# this does an incremental build of the haskell code for faster testing
echo "testing build in nix-shell..."
cmd="(stack build $STACK_ARGS && stack $STACK_ARGS exec shortcut -- $TEST_ARGS) || exit"
nix-shell $NIX_ARGS --command "$cmd" 2>&1 | tee test-stack.log

# this builds everything at once, which is simpler.
# the downside is it rebuilds the haskell code from scratch.
echo "testing nix-only build (no stack)..."
(nix-build $NIX_ARGS && ./result/bin/shortcut $TEST_ARGS) 2>&1 | tee test-nix.log
