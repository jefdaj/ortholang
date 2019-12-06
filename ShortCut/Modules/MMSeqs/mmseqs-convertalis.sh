#!/usr/bin/env bash

# TODO does my .out, .err convention mess with the .index convention?

set -e

OUTTAB="$1"
QDB="$2"
SDB="$3"
ODB="$4"

# mmseqs will write to "${ODB}.index" on its own, and other files
mmseqs convertalis "$QDB" "$SDB" "$ODB" "$OUTTAB" > "${OUTTAB}.out" 2> "${OUTTAB}.err"
