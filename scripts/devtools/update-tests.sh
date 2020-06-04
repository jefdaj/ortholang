#!/usr/bin/env bash

# Usage:
# ./scripts/devtools/update-tests.sh [filter]
#
# Optional test filter follows this format:
# https://ro-che.info/articles/2018-01-08-tasty-new-patterns
#
# For example:
# ./scripts/dev/update-tests.sh '$2 ~/version/ || $2 ~/repl/ || $2 ~/parser/'
#
# while that runs, periodically:
#   git diff tests
#   check that the changes make sense
#   git add tests

set -e
set -x

logfile="test.log"
testdir="tests"
[[ -z "$1" ]] && testfilter="all" || testfilter="$1"

nix-shell --command 'stack --allow-different-user build'

stacktestdir="$(find .stack-work -type d -name tests)"

# delete golden files so they'll be recreated
for d in ${stacktestdir}/*; do
  [[ $d =~ 'scripts' || $d =~ 'repl' ]] || rm -rf  $d/*
done

rsync_tests() { rsync -qr ${stacktestdir}/* ${testdir}/; }

rsync_every_10sec() { while sleep 10; do rsync_tests || break; done || true; }

# run tests to remake them, while syncing back to main tests dir
rsync_every_10sec &>/dev/null &

nix-shell --command "stack --allow-different-user exec ortholang -- --test '$testfilter' 2>&1 | tee '$logfile'"

# one last clean rsync
kill -9 $rsync_pid || true &> /dev/null
rsync_tests
