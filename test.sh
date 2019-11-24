#!/usr/bin/env bash

# remember to export TMPDIR=<some shared location> before testing on HPC clusters!
# script arguments will be passed to shortcut --test
# other possible tasty settings: https://hackage.haskell.org/package/tasty

test_branch='all'

set -e
set -o pipefail

export TASTY_QUICKCHECK_TESTS=100
export TASTY_COLOR="always"
export TASTY_QUICKCHECK_SHOW_REPLAY=True
# export TASTY_HIDE_SUCCESSES=True
# [[ -z "$TMPDIR" ]] && export TMPDIR=/tmp

timestamp=$(date '+%Y-%m-%d_%H:%M')

nix_args="--pure -j2"
stack_cmd="stack build && stack exec shortcut -- --test $test_branch"

log="shortcut-${test_branch}-${timestamp}.log"

set -x
nix-shell $nix_args --command "$stack_cmd" 2>&1 | tee -a $log

# log="shortcut-test_${timestamp}.log"
# test_args="+RTS -IO -N -RTS --test biomartr"
# cmd="./result/bin/shortcut $test_args"
# echo "$cmd" | tee $log
# $cmd 2>&1 | tee -a $log
