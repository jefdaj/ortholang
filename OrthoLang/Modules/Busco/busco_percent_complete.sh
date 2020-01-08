#!/usr/bin/env bash

# TODO need to do a separate .out file for consistency?

OUTPATH="$1"
SUMMARY="$2"

egrep -o 'C:([0-9\.]*)\%' "$SUMMARY" | cut -d':' -f2- | cut -d '%' -f1 > "$OUTPATH" 2> "${OUTPATH}.err"
