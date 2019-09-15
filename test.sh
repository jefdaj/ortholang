#!/usr/bin/env bash

# remember to export TMPDIR=<some shared location> before testing on HPC clusters!
# script arguments will be passed to shortcut --test
# other possible tasty settings: https://hackage.haskell.org/package/tasty

set -E

export NIX_ARGS="--pure -j$(nproc)"
export TASTY_QUICKCHECK_TESTS=1000
export TASTY_COLOR="always"
export TASTY_QUICKCHECK_SHOW_REPLAY=True
# export TASTY_HIDE_SUCCESSES=True
[[ -z "$TMPDIR" ]] && export TMPDIR=/tmp

log="shortcut-build_$(date '+%Y-%m-%d_%H:%M').log"
cmd="nix-build $NIX_ARGS"
echo "$cmd" | tee $log
$cmd 2>&1 | tee -a $log

log="shortcut-test_$(date '+%Y-%m-%d_%H:%M').log"
TEST_ARGS="+RTS -IO -N -RTS --test $@"
cmd="./result/bin/shortcut $TEST_ARGS"
echo "$cmd" | tee $log
$cmd 2>&1 | tee -a $log
