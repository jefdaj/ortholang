#!/usr/bin/env bash

outpath="$1"
faapath="$2"
diamond makedb --in "$faapath" --db "$outpath"
