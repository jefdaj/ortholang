#!/usr/bin/env bash

set -e

OPATH="$1"
TDIR="$2" # TODO remove?
QSRC="$3"
TSRC="$4"

cd "$TDIR"
crb-blast -q "$QSRC" -t "$TSRC" -o "$OPATH" > "${OPATH}.out" 2> "${OPATH}.err"
