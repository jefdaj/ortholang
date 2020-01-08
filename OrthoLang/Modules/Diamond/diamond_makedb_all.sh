#!/usr/bin/env bash

set -e

outpath="$1"; shift
faapaths="$@"

cat $faapaths | diamond makedb --db "$outpath" > "${outpath}.out" 2> "${outpath}.err"
