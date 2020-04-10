#!/usr/bin/env bash

set -e

DBDIR="$1"

cd "$DBDIR"
blastdbcmd -info -db "result"
