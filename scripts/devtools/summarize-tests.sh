#!/usr/bin/env bash

LOG=test.log

echo -e "tests passed:           \t$(grep OK $LOG | wc -l)"
echo
echo -e "tests timed out:        \t$(grep TIMEOUT $LOG | wc -l)"
echo -e "important tests failed: \t$(grep FAIL $LOG | grep -v help | wc -l)"
echo -e "trivial tests failed:   \t$(grep FAIL $LOG | grep    help | wc -l)"
echo
echo -e "check changed:          \t$(git status | grep tests | grep check | wc -l)"
echo -e "expand changed:         \t$(git status | grep tests | grep expand | wc -l)"
echo -e "help changed:           \t$(git status | grep tests | grep help | wc -l)"
echo -e "parse changed:          \t$(git status | grep tests | grep parse | wc -l)"
echo -e "repl changed:           \t$(git status | grep tests | grep repl | wc -l)"
echo -e "scripts changed:        \t$(git status | grep tests | grep scripts | wc -l)"
echo -e "stdout changed:         \t$(git status | grep tests | grep stdout | wc -l)"
echo -e "tmpfiles changed:       \t$(git status | grep tests | grep tmpfiles | wc -l)"
echo -e "versions changed:       \t$(git status | grep tests | grep versions | wc -l)"
