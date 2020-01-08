#!/usr/bin/env bash

set -e

DBPATH="$1"; shift
FAPATHS="$@"

# mmseqs will write to "${DBPATH}.mmseqs2db.index" on its own, and other files
mmseqs createdb $FAPATHS "$DBPATH" > "${DBPATH}.out" 2> "${DBPATH}.err"
