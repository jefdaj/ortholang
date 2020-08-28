#!/usr/bin/env bash

set -e

# ODIR="$(dirname "$1")"; shift
# QPATH="$1"; shift
# ESTR="$1"; shift
# DBPATH="$1"; shift
# DCMD="$@"
# 
# mkdir -p "$ODIR"
# cd "$ODIR"
# diamond $DCMD -q "$QPATH" -o tmp -e "$ESTR" -d "$DBPATH" > out 2> err && mv tmp result
# 
# # this works around a bug where it would sometimes crash and leave a partially written table
# # TODO remove when sure that's fixed
# sync
# last_line="$(tail -n1 result)"
# n_tabs="$(echo "$last_line" | sed 's/[^\t]//g' | awk '{print length}')"
# if [[ $n_tabs != 11 ]]; then
#   echo "error! ${ODIR}/result is incomplete. last line has ${n_tabs} tabs:\n${last_line}"
#   rm -f result tmp
#   exit 1
# fi
