#!/usr/bin/env bash

OUTPREFIX="$1" # TODO full path?
INPATH="$2"
LINEAGE="$3" # TODO full path without extension i think
MODE="$4"
TDIR="$5"

OBASE="$(basename "$OUTPREFIX")"
# LDIR="$(dirname $(dirname "$LINEAGE"))"
LDIR="$LINEAGE"
LNAME="$(basename "$LINEAGE")"

# some of these seem to be required,
# even though they're supposedly overridden by the cli.
CFGPATH="${OUTPREFIX}.ini"
cat << EOF > "$CFGPATH"
[busco]
in           = "$INPATH"
out_path     = "$OUTPREFIX"
lineage_path = "$LDIR"
cpu          = $(nproc)
tmp_path     = "$TDIR"
quiet        = False
gzip         = False
# TODO set these? or does lineage handle it?
# domain     = 
# species    = 
EOF

CFGTEMPLATE="@busco@/config/config.ini"
tail -n +45 "$CFGTEMPLATE" >> "$CFGPATH"

export BUSCO_CONFIG_FILE="$CFGPATH"
run_BUSCO.py --cpu $(nproc) -i "$INPATH" -o "$OBASE" -l "$LINEAGE" -m "$MODE" > "${OUTPREFIX}.out" 2> "${OUTPREFIX}.err"
