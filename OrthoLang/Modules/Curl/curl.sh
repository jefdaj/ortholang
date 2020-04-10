#!/usr/bin/env bash

OUTPATH="$1"
ERRPATH="$(dirname "$OUTPATH")"/err
URL="$2"

# from https://www.client9.com/using-curl-in-automation/
curl --silent --show-error -L --max-redirs 3 \
  --retry 3 --retry-connrefused --retry-delay 2 --retry-max-time 60 \
  "$URL" -o "$OUTPATH" &> "$ERRPATH"
