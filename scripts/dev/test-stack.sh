#!/usr/bin/env bash

set -e
set -x
set -o pipefail

# only run tests matching this filter string:
BRANCH="$(git rev-parse --abbrev-ref HEAD | cut -d'-' -f2-)"
[[ -z "$1" ]] && TEST_FILTER="$BRANCH" || TEST_FILTER="$1"

if [[ -z "$TMPDIR" ]]; then
  export TMPDIR=$PWD/.stack-work/tmp
  mkdir -p "$TMPDIR"
fi

### build the binary ###

NIX_ARGS="--pure"
TIMESTAMP=$(date '+%Y-%m-%d_%H:%M')
LOGFILE="ortholang_${TEST_FILTER}_${TIMESTAMP}.log"

nix-run() {
  nix-shell shell.nix $NIX_ARGS --run "$@" 2>&1 | tee -a "$LOGFILE"
}

nix-run "stack build"

### run tests ###

# other possible tasty settings: https://hackage.haskell.org/package/tasty
export TASTY_QUICKCHECK_TESTS=1000
export TASTY_COLOR="always"
export TASTY_QUICKCHECK_SHOW_REPLAY=True

STACK_CMD="stack exec ortholang --"
TEST_ARGS="--test '$TEST_FILTER'"

# test using shared cache first because it's faster
nix-run "$STACK_CMD --shared http://shortcut.pmb.berkeley.edu/shared $TEST_ARGS"

# then locally to verify everything really works
nix-run "$STACK_CMD $TEST_ARGS"
