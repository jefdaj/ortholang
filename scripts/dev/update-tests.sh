#!/usr/bin/env bash

# Usage:
# 1) nix-shell
# 2) ./scripts/dev/update-tests.sh
# 3) while that runs, periodically:
#      git diff tests
#      check that the changes make sense
#      git add tests

set -e

stack build

logfile="test.log"
testdir="tests"
stacktestdir="$(find .stack-work -type d -name tests)"

# delete golden files
for d in ${stacktestdir}/*; do
  [[ $d =~ 'scripts' ]] || rm -rf  $d/*
done

update_tests() {
  while sleep 10; do
    rsync -r ${stacktestdir}/* ${testdir}/
  done
}

# run tests to remake them, while syncing back to main tests dir
update_tests &
pid=$?
stack exec ortholang -- --test all 2>&1 | tee "$logfile"

# wait for one last rsync, then kill it
sleep 15
kill -9 $pid || true
