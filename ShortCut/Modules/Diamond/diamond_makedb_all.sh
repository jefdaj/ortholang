#!/usr/bin/env bash

outpath="$1"; shift
faapaths="$@"
cat $faapaths | diamond makedb --db "$outpath"
