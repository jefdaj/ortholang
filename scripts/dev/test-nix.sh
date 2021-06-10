#!/usr/bin/env bash

set -x
set -o pipefail

# TIMESTAMP=$(date '+%Y-%m-%d_%H:%M')
# LOGFILE="ortholang_${testfilter}_${TIMESTAMP}.log"
LOGFILE='test.log'
rm -f "$LOGFILE" # helps prevent unneccesary nix-builds

# figure out what branch we're on
branch="$TRAVIS_BRANCH"
[[ -z "$branch" ]] && branch="$(git rev-parse --abbrev-ref HEAD)"

# run module-specific tests only on module branches,
# and the standard interpreter tests everywhere else
# TODO how to exclude things here that are the matched name + something more?
[[ "$branch" =~ module ]] \
  && testfilter="\$0 ~ /.$(echo "$branch" | cut -d'-' -f2):/ || \$0 ~ /'$(echo "$branch" | cut -d'-' -f2)'/" \
  || testfilter='$2 ~/version/ || $2 ~/repl/ || $2 ~/parser/ || $5 ~/parses/ || $5 ~/expands/'

# unless it's something important,
[[ "$branch" =~ '^(master|bugfix)' ]] && testfilter="all"

# or one of these specific cases.
[[ "$branch" =~ '^(feature-nix-tooling|develop)$' ]] && testfilter='$2 ~/version/'

# finally, override from the command line
[[ -z "$1" ]] || testfilter="$1"

if [[ -z "$TMPDIR" ]]; then
  export TMPDIR=$PWD/.stack-work/tmp
  mkdir -p "$TMPDIR"
fi

### build the binary ###

NIX_ARGS="" # TODO put back --pure?

nix-build release.nix $NIX_ARGS
code0=$?
[[ $code0 == 0 ]] || exit $code0

# bin-run() {
#   rm -f $LOGFILE
#   ./result/bin/ortholang $@ 2>&1 | tee -a $LOGFILE
#   code="$?"
#   [[ $code == 0 ]] || cat $LOGFILE
#   return $code
# }

### run tests ###

# other possible tasty settings: https://hackage.haskell.org/package/tasty
export TASTY_QUICKCHECK_TESTS=1000
export TASTY_COLOR="always"
export TASTY_QUICKCHECK_SHOW_REPLAY=True

# TEST_ARGS=bin-run "debug '.*' --test $testfilter"
# test using shared cache first because it's faster
# TODO put back once server is back up
# bin-run --shared http://shortcut.pmb.berkeley.edu/shared $TEST_ARGS
# code1="$?"
# then locally to verify everything really works
# bin-run $TEST_ARGS
# code2="$?"
# exit nonzero if either run failed
# [[ $code1 == 0 ]] || exit $code1
# exit $code2

# local tests only pending server update
./result/bin/ortholang --debug '.*' --test "$testfilter" 2>&1 | tee -a $LOGFILE
