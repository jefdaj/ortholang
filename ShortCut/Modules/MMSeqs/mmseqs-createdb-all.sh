#!/usr/bin/env bash

# TODO does my .out, .err convention mess with the .index convention?

set -e

DBPATH="$1"; shift
CACHEDIR="$1"; shift
FAPATHS="$@"

# mmseqs will write to "${DBPATH}.index" on its own, and other files
cd "$CACHEDIR" # TODO remove?
mmseqs createdb $FAPATHS "$DBPATH" > "${DBPATH}.out" 2> "${DBPATH}.err"
