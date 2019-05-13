#!/usr/bin/env bash

set -e

OUTDB="$1"
TDIR="$2"
ESTR="$3"
QDB="$4"
SDB="$5"

mmseqs search -e "$ESTR" "$QDB" "$SDB" "$ODB" "$TDIR" > "${ODB}.out" 2> "${ODB}.err"
rm -rf "$TDIR" # TODO from haskell?
