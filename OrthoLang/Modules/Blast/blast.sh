#!/nix/store/rm1hz1lybxangc8sdl7xvzs5dcvigvf7-bash-4.4-p23/bin/bash

set -e

OUTDIR="$(dirname "$1")" # TODO fix this pattern
BLASTCMD="$2"
EDEC="$3" # this is the actual evalue, not a path
QPATH="$4"
PPATH="$5" # TODO why not QPATH?

# TODO remove once all bugs are clearly worked out?
if [[ -d "$OUTDIR" && "$(ls "$OUTDIR" | wc -l)" -gt 0 ]]; then
  echo "ERROR! old results remain in '$OUTDIR':"
	ls -al "$OUTDIR"
	exit 1
fi

DBDIR="$(dirname "$PPATH")"
DBNAME="$(basename "$PPATH")"

cd "$DBDIR" # TODO remove?
export BLASTDB="$DBNAME" # TODO DBPATH?

# Keep sequence of output same as the order of input. Normally the output of a
# job will be printed as soon as the job completes.
PARCMD="parallel -k --group"

# This should help prevent overloading the system when Shake runs multiple parallel BLASTs at once.
PARCMD="$PARCMD --load 90% --noswap"

# When using --pipepart a negative block size is not interpreted as a blocksize
# but as the number of blocks each jobslot should have. This is an efficient
# alternative to --roundrobin because data is never read by GNU parallel, but
# you can still have very few jobslots process a large amount of data.
# TODO is 10 a good number? will result in roughly 10 * nproc processes per blast cmd
PARCMD="$PARCMD -a $QPATH --pipepart -j$(nproc) --block -10 --cat"

PARCMD="$PARCMD --cat --recstart > --joblog ${OUTDIR}/jobs --halt now,fail=1 --will-cite"
PARCMD="$PARCMD $BLASTCMD -db $DBNAME -evalue $EDEC -outfmt 6 -query"

# echo "PARCMD: $PARCMD"

$PARCMD > "${OUTDIR}/out" 2> "${OUTDIR}/err"

if [[ -z "${OUTDIR}/out" ]]; then
  echo "<<emptybht>>" > "${OUTDIR}/result"
else
  cd "$OUTDIR"
  ln -s out result
fi
