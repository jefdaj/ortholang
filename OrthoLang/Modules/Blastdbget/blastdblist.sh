#!/usr/bin/env bash

# set -e

TDIR="$1" # TODO remove?
LISTRES="$2"
LISTOUT="${2}.out"
LISTERR="${2}.err"

cd "$TDIR"

blastdbget "$TDIR" > "$LISTOUT" 2> "$LISTERR"

# to get the list of databases, you call blastdbget without the proper args
# so we actually have to check that it fails here...
if [[ ! $? -eq 1 ]]; then
  cat "$LISTERR"
  exit 1
fi

# ... and then filter out the usage info + non-tunable log output
grep -v 'INFO' "$LISTOUT" | grep -v 'Usage:' > "$LISTRES"
exit 0

# TODO remove the .out and .err files if empty?
