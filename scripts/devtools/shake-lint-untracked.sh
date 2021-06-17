#!/usr/bin/env bash

# usage:
# 1) set shakeLint = Just ... in Eval.hs
# 2) stack build
# 3) stack exec ortholang -- --test whatever 2>&1 | tee test.log
# 4) ./scripts/devtools/shake-lint-untracked.sh to grep the log and list untracked files

LOG=test.log

egrep '(Key|Used|Created):' $LOG | cut -d':' -f2- | sort | uniq
