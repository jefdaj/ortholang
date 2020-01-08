#!/usr/bin/env bash

set -e

outpath="$1"
faapath="$2"

diamond makedb --in "$faapath" --db "$outpath" > "${outpath}.out" 2> "${outpath}.err"
