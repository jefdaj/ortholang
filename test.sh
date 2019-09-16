#!/usr/bin/env bash

# remember to export TMPDIR=<some shared location> before testing on HPC clusters!
# script arguments will be passed to shortcut --test
# other possible tasty settings: https://hackage.haskell.org/package/tasty

set -e

timestamp=$(date '+%Y-%m-%d_%H:%M')

export NIX_PATH=nixpkgs=channel:nixos-19.03
export nix_args="--pure -j2"

log="shortcut-build_${timestamp}.log"
cmd="nix-build $nix_args"
echo "$cmd" | tee $log
$cmd 2>&1 | tee -a $log

export TASTY_QUICKCHECK_TESTS=100
export TASTY_COLOR="always"
export TASTY_QUICKCHECK_SHOW_REPLAY=True
# export TASTY_HIDE_SUCCESSES=True
# [[ -z "$TMPDIR" ]] && export TMPDIR=/tmp

log="shortcut-test_${timestamp}.log"
test_args="+RTS -IO -N -RTS --test $@"
cmd="./result/bin/shortcut $test_args"
echo "$cmd" | tee $log
$cmd 2>&1 | tee -a $log
