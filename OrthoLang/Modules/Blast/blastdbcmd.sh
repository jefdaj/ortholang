#!/usr/bin/env bash

set -e

DBDIR="$1"
DBBASE="$2"

cd "$DBDIR"
blastdbcmd -info -db "$DBBASE"
