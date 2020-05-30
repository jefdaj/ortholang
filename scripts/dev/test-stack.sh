#!/usr/bin/env bash

set -x
set -o pipefail

# start with the basic tests
TEST_FILTER='$2 ~/version/ || $2 ~/repl/ || $2 ~/parser/ || $5 ~/parses/ || $5 ~/expands/'

# add module-specific scripts if any
BRANCH="$(git rev-parse --abbrev-ref HEAD)"
[[ "$BRANCH" =~ module ]] \
  && TEST_FILTER="$TEST_FILTER || \$5 ~/$(echo "$BRANCH" | cut -d'-' -f2):/"

# override from the command line
[[ -z "$1" ]] || TEST_FILTER="$1"

if [[ -z "$TMPDIR" ]]; then
  export TMPDIR=$PWD/.stack-work/tmp
  mkdir -p "$TMPDIR"
fi

### build the binary ###

NIX_ARGS="" # TODO put back --pure?
# TIMESTAMP=$(date '+%Y-%m-%d_%H:%M')
# LOGFILE="ortholang_${TEST_FILTER}_${TIMESTAMP}.log"
LOGFILE='test.log'

nix-run() {
  rm -f "$LOGFILE"
  nix-shell shell.nix $NIX_ARGS --run "$@" 2>&1 | tee -a "$LOGFILE"
  code="$?"
  [[ $code == 0 ]] || cat "$LOGFILE"
  return $code
}

nix-run "stack build"

### run tests ###

# other possible tasty settings: https://hackage.haskell.org/package/tasty
export TASTY_QUICKCHECK_TESTS=1000
export TASTY_COLOR="always"
export TASTY_QUICKCHECK_SHOW_REPLAY=True

STACK_CMD="stack exec ortholang --"
TEST_ARGS="--debug '.*' --test '$TEST_FILTER'"

# test using shared cache first because it's faster
# nix-run "$STACK_CMD --shared http://shortcut.pmb.berkeley.edu/shared $TEST_ARGS"
# code1="$?"
# then locally to verify everything really works
# nix-run "$STACK_CMD $TEST_ARGS"
# code2="$?"
# exit nonzero if either run failed
# [[ $code1 == 0 ]] || exit $code1
# exit $code2

# local tests only pending server update
nix-run "$STACK_CMD $TEST_ARGS"
