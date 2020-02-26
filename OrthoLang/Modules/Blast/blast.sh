#!/nix/store/rm1hz1lybxangc8sdl7xvzs5dcvigvf7-bash-4.4-p23/bin/bash

# TODO query on stdin like the current haskell code?
# TODO should this .out + .err + mv thing be a regular practice everywhere?
# TODO and if so, maybe it should be done in haskell? only after it works a couple times the tedious way

set -e

OUTDIR="$(dirname "$1")" # TODO fix this pattern
BLASTCMD="$2"
EDEC="$3" # this is the actual evalue, not a path
QPATH="$4"
PPATH="$5" # TODO why not QPATH?

DBDIR="$(dirname "$PPATH")"
DBNAME="$(basename "$PPATH")"

cd "$DBDIR" # TODO remove?
export BLASTDB="$DBNAME" # TODO DBPATH?

# PARCMD="parallel --group --pipe --round-robin -j 0 --joblog ${OUTDIR}/joblog"
# PARCMD="$PARCMD --halt now,fail=1 --recstart '>' --will-cite --block-size 1k --load 90% --noswap"

# Keep sequence of output same as the order of input. Normally the output of a
# job will be printed as soon as the job completes.
PARCMD="parallel -j$(nproc) -u"

# When using --pipepart a negative block size is not interpreted as a blocksize
# but as the number of blocks each jobslot should have. This is an efficient
# alternative to --roundrobin because data is never read by GNU parallel, but
# you can still have very few jobslots process a large amount of data.
# TODO is 10 a good number?
PARCMD="$PARCMD -a $QPATH --pipepart --block -10 --cat"

PARCMD="$PARCMD --cat --recstart > --joblog ${OUTDIR}/jobs --halt now,fail=1 --will-cite"
PARCMD="$PARCMD $BLASTCMD -db $DBNAME -evalue $EDEC -outfmt 6 -query"
# PARCMD="$PARCMD wc -l"

# echo "version: $(parallel --version)"
echo "PARCMD: $PARCMD"

$PARCMD > "${OUTDIR}/out" 2> "${OUTDIR}/err"

if [[ -z "${OUTDIR}/out" ]]; then
  echo "<<emptybht>>" > "${OUTDIR}/result"
else
  cd "$OUTDIR"
  ln -s out result
fi
