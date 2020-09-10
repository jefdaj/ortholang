#!/usr/bin/env bash

# set -e

TDIR="$1" # TODO remove?
LISTTMP="$2"

cd "$TDIR"

# ok, internet is working. so why no connection to ncbi?
# TODO oh, the files are gone from ncbi ftp? find them
# ping -c1 -w1 google.com

blastdbget "$TDIR" > "$LISTTMP" 2> "${LISTTMP}.err" || true

# to get the list of databases, you call blastdbget without the proper args
# so we actually have to check that it fails here...
if [[ ! $? -eq 1 ]]; then
  cat "${LISTTMP}.err"
  exit 1
fi

# ... and then filter out the usage info + non-tunable log output
grep -v 'INFO' "$LISTTMP" | grep -v 'Usage:' > "${LISTTMP}.out"
