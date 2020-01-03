#!/usr/bin/env bash

# remember to export TMPDIR=<some shared location> before testing on HPC clusters!
# script arguments will be passed to shortcut --test
# other possible tasty settings: https://hackage.haskell.org/package/tasty

test_filter='version'

set -e
set -o pipefail

export TASTY_QUICKCHECK_TESTS=100
export TASTY_COLOR="always"
export TASTY_QUICKCHECK_SHOW_REPLAY=True
# export TASTY_HIDE_SUCCESSES=True
# [[ -z "$TMPDIR" ]] && export TMPDIR=/tmp

timestamp=$(date '+%Y-%m-%d_%H:%M')

nix_args="--pure -j2"
stack_cmd="stack build && stack exec shortcut -- --test $test_filter"

log="shortcut-${test_filter}-${timestamp}.log"

set -x
nix-shell $nix_args --command "$stack_cmd; exit" 2>&1 | tee -a $log

# test with the demo server cache too
# (just blastp functions for now to keep from getting too big)
stack_cmd_2="stack exec shortcut -- --shared http://shortcut.pmb.berkeley.edu/shared --test version'"
nix-shell $nix_args --command "$stack_cmd_2; exit" 2>&1 | tee -a $log

# log="shortcut-test_${timestamp}.log"
# test_args="+RTS -IO -N -RTS --test biomartr"
# cmd="./result/bin/shortcut $test_args"
# echo "$cmd" | tee $log
# $cmd 2>&1 | tee -a $log
