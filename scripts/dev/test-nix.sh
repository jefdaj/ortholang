#!/usr/bin/env bash

set -e
set -x
set -o pipefail

# only run tests matching this filter string:
BRANCH="$(git rev-parse --abbrev-ref HEAD)"
[[ "$BRANCH" =~ module ]] && DEFAULT_FILTER="$(echo "$BRANCH" | cut -d'-' -f2)" || DEFAULT_FILTER='all'
[[ -z "$1" ]] && TEST_FILTER="$DEFAULT_FILTER" || TEST_FILTER="$1"

if [[ -z "$TMPDIR" ]]; then
  export TMPDIR=$PWD/.stack-work/tmp
  mkdir -p "$TMPDIR"
fi

### build the binary ###

NIX_ARGS="--pure"
TIMESTAMP=$(date '+%Y-%m-%d_%H:%M')
LOGFILE="ortholang_${TEST_FILTER}_${TIMESTAMP}.log"

nix-build $NIX_ARGS 2>&1 | tee -a $LOGFILE

export PATH=$PWD/result/bin:$PATH

### run tests ###

# other possible tasty settings: https://hackage.haskell.org/package/tasty
export TASTY_QUICKCHECK_TESTS=1000
export TASTY_COLOR="always"
export TASTY_QUICKCHECK_SHOW_REPLAY=True

# test using shared cache first because it's faster
ortholang \
  --shared http://shortcut.pmb.berkeley.edu/shared \
  --test "$TEST_FILTER" 2>&1 | tee -a $LOGFILE

# then locally to verify everything really works
ortholang --test "$TEST_FILTER" 2>&1 | tee -a $LOGFILE
