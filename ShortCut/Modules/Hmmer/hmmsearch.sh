#!/usr/bin/env bash

# TODO anything special needed because this writes two files?
# TODO maybe simplify it so there's only one set of .out + .err

set -e

OUTPATH="$1"
EDEC="$2"
TMPOUT="$3"
HMMPATH="$4"
FAPATH="$5"

hmmsearch -E "$ESTR" --tblout "$TMPOUT" "$HMMPATH" "$FAPATH" > "${TMPOUT}.out" 2> "${TMPOUT}.err"
sed '/^#/d' "$TMPOUT" > "${OUTPATH}.out" 2> "${OUTPATH}.err"
ln -s "${OUTPATH}.out" "$OUTPATH"
