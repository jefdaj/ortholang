#!/usr/bin/env bash

LOG=test.log

echo "tests passed: $(grep OK $LOG | wc -l)"
echo
echo "tests timed out: $(grep TIMEOUT $LOG | wc -l)"
echo "important tests failed: $(grep FAIL $LOG | grep -v help | wc -l)"
echo "trivial tests failed: $(grep FAIL $LOG | grep    help | wc -l)"
echo
echo "tmpfiles changed: $(git status | grep tests | grep tmpfiles | wc -l)"
echo "stdouts changed: $(git status | grep tests | grep stdout | wc -l)"
echo "expands changed: $(git status | grep tests | grep expand | wc -l)"
