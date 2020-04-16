#!/usr/bin/env bash

# usage:
# 1) set shakeLint = Just ... in Eval.hs
# 2) stack build
# 3) stack exec ortholang -- --test whatever 2>&1 | tee test.log
# 4) ./scripts/dev/shake-lint-untracked.sh to grep the log and list untracked files

LOG=test.log

egrep '(Rule|Created):' $LOG |
  sed 's/^\s*//g' |
  sed 's:/home.*/exprs:exprs:g' |
  sed 's:/home.*/cache:cache:g' |
  sed 's:Rule:\nRule:g' > shake-lint-untracked.txt
