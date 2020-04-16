#!/usr/bin/env bash

LOG=test.log

echo -e "tests passed:\t$(grep OK $LOG | wc -l)"
echo
echo -e "tests timed out:\t$(grep TIMEOUT $LOG | wc -l)"
echo -e "important tests failed:\t$(grep FAIL $LOG | grep -v help | wc -l)"
echo -e "trivial tests failed:\t$(grep FAIL $LOG | grep    help | wc -l)"
echo
echo -e "tmpfiles changed:\t$(git status | grep tests | grep tmpfiles | wc -l)"
echo -e "stdouts changed:\t$(git status | grep tests | grep stdout | wc -l)"
echo -e "expands changed:\t$(git status | grep tests | grep expand | wc -l)"
