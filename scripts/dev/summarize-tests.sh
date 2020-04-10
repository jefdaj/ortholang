#!/usr/bin/env bash

LOG=test.log

echo "passed: $(grep OK $LOG | wc -l)"
echo "failed: $(grep FAIL $LOG | wc -l)"
echo "timeout: $(grep TIMEOUT $LOG | wc -l)"

echo "tmpfiles changed: $(git status | grep tests | grep tmpfiles | wc -l)"
echo "stdouts changed: $(git status | grep tests | grep stdout | wc -l)"
echo "expands changed: $(git status | grep tests | grep expand | wc -l)"
