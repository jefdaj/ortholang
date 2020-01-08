#!/usr/bin/env bash

# TODO any special need to retry a couple times before giving up in case of bad internet here?

OUTPATH="$1"
URL="$2"
VERBOSE="$3"

curl -L $VERBOSE "$URL" -o "$OUTPATH"
