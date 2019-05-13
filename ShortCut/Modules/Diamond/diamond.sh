#!/usr/bin/env bash

set -e

OPATH="$1"; shift
QPATH="$1"; shift
ESTR="$1"; shift
DBPATH="$1"; shift
DCMD="$@"

diamond $DCMD -q "$QPATH" -o "$OPATH" -e "$ESTR" -d "$DBPATH" > "${OPATH}.out" 2> "${OPATH}.err"
