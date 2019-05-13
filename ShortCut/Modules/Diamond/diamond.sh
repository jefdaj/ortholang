#!/usr/bin/env bash

set -e

OPATH="$1"
QPATH="$2"
ESTR="$3"
DBPATH="$4"

diamond -q "$QPATH" -o "$OPATH" -e "$ESTR" -d "$DBPATH" > "${OPATH}.out" 2> "${OPATH}.err"
