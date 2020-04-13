#!/usr/bin/env bash

set -e

OPATH="$1"
TDIR="$2" # TODO remove?
QSRC="$3"
TSRC="$4"

# echo "OPATH: $OPATH"
# echo "QSRC: $QSRC"
# echo "TDIR: $TDIR"
# echo "TSRC: $TSRC"

cd "$TDIR"
crb-blast -q "$QSRC" -t "$TSRC" -o "$OPATH" > "${OPATH}.out" 2> "${OPATH}.err"

# code="$?"
# cp "$OPATH.out" /tmp/crbblast-out.txt
# cp "$OPATH.err" /tmp/crbblast-err.txt
# exit $code
