#!/usr/bin/env bash

set -e

OUTDB="$1"
TDIR="$2"
ESTR="$3"
QDB="$4"
SDB="$5"

cmd="mmseqs search -e "$ESTR" "$QDB" "$SDB" "$OUTDB" "$TDIR" > "${OUTDB}.out" 2> "${OUTDB}.err""
echo "$cmd"
eval "$cmd"
rm -rf "$TDIR" # TODO from haskell?
