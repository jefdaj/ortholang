#!/usr/bin/env bash

# remember to export TMPDIR=<some shared location> before testing on HPC clusters!
# script arguments will be passed to shortcut --test
# other possible tasty settings: https://hackage.haskell.org/package/tasty

export TASTY_QUICKCHECK_TESTS=1000
export TASTY_COLOR="always"
export TASTY_QUICKCHECK_SHOW_REPLAY=True
export TASTY_HIDE_SUCCESSES=True
[[ -z "$TMPDIR" ]] && export TMPDIR=/tmp

# this builds everything at once, which is simpler.
# the downside is it rebuilds the haskell code from scratch.
echo "testing nix build..."
(nix-build -j$(nproc) && ./result/bin/shortcut --test --pattern output $@ +RTS -IO -N -RTS) 2>&1 | tee nix-build.log

# this does an incremental build of the haskell code for faster testing
echo "testing stack build..."
export STACK_ROOT=$PWD/.stack-work
mkdir -p $STACK_ROOT
cmd='(stack build --allow-different-user && ./.stack-work/install/*/*/*/bin/shortcut --test) || exit'
nix-shell --command "$cmd" 2>&1 | tee stack-build.log
