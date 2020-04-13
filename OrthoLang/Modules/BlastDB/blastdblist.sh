#!/usr/bin/env bash

# full db prefix is $TDIR/$DBNAME

# set -e

TDIR="$1"
LISTTMP="$2"

cd "$TDIR"

blastdbget "$TDIR" > "$LISTTMP" 2> "${LISTTMP}.err"

# to get the list of databases, you call blastdbget without the proper args
# so we actually have to check that it fails here...
[[ $? -eq 1 ]] || (echo "blastdbget failed" && exit 1)

# ... and then filter out the usage info + non-tunable log output
grep -v 'INFO' "$LISTTMP" | grep -v 'Usage:' > "${LISTTMP}.out"
