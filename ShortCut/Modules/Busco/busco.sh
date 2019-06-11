#!/usr/bin/env bash

# note there's some weirdness with the outpaths.
# busco will generate its own hashed outdir, so we just pass it the tmpdir.
# then in haskell the generated run_*.bur dir is symlinked to the final outpath

OUTPREFIX="$1" # busco itself doesn't get to see this
INPATH="$2"
LINEAGE="$3"
MODE="$4"
TDIR="$5"

OPATH="${TDIR}/$(basename "$OUTPREFIX")"
OBASE="$(basename "$OPATH")"

# some of these seem to be required,
# even though they're supposedly overridden by the cli.
CFGPATH="${OUTPREFIX}.ini"
cat << EOF > "$CFGPATH"
[busco]
in           = $INPATH
out_path     = $OPATH
lineage_path = $LINEAGE
cpu          = $(nproc)
tmp_path     = $TDIR/$(basename "$OUTPREFIX")
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
