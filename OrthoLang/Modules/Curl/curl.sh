#!/usr/bin/env bash

OUTPATH="$1"
ERRPATH="${OUTPATH}.err"
URLPATH="$2"
URL="$(cat "$URLPATH")"

# echo "OUTPATH: $OUTPATH"
# echo "ERRPATH: $ERRPATH"
# echo "URLPATH: $URLPATH"
# echo "URL: $URL"

# from https://www.client9.com/using-curl-in-automation/
curl --silent --show-error -L --max-redirs 3 \
  --retry 3 --retry-connrefused --retry-delay 2 --retry-max-time 60 \
  "$URL" -o "$OUTPATH" &> "$ERRPATH"

# remove if empty
[[ -s "$ERRPATH" ]] || rm "$ERRPATH"
