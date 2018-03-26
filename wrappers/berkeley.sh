#!/bin/bash

# Wrapper for the Berkeley Computational Genomics Resource Laboratory (CGRL).
# It should work with minor changes on any system that uses the SLURM scheduler
# though if you adjust the SRUN variable below.

# Note that you also need to set TMPDIR to something shared across machines
# before invoking shortcut. For example I use:
# export TMPDIR=/global/scratch/jefdaj/shortcut-test
# Otherwise you'll get lots of srun errors "cannot chdir to /tmp/whatever..."

# TODO offload hashing with srun_crb too if possible
# TODO control number of parallel jobs in srun_parallel rather than haskell!

# Common options for all srun commands
SRUN="srun --account=co_rosalind --partition=savio2_htc --qos=rosalind_htc2_normal"
SRUN="$SRUN --chdir $(pwd) --quiet"
# TODO --exclusive -N1 -n1?

srun_crb() {
  srun="$SRUN --cpus-per-task=7 --nodes=1-1 --ntasks=1 --mem=50G --time=99:00:00"
  echo "$srun $@"
}

srun_quick() {
  echo "$SRUN --cpus-per-task=1 --nodes=1-1 --ntasks=1 --time=00:10:00 $@"
}

srun_parallel() {
  # monkey-patches a parallel call to run its individual commands via slurm
  # note that it's brittle and only works on shortcut-generated blast commands
  cmd="$@"
  before="$(echo "$cmd" | cut -d' ' -f-10)" # ... --pipe
  after="$(echo "$cmd" | cut -d' ' -f11-)"  # '*blast* ...
  pargs="--block 100k -j50 --delay 1" # additional parallel args
  srun="$SRUN --cpus-per-task=1 --nodes=1-1 --ntasks=1 --time=99:00:00"
  cmd="${before} ${pargs} ${srun} ${after}"
  echo "$cmd"
}

# If none of the special cases below match, this will run as-is.
CMD="$@"

# Make parallel blast run individual commands via srun.
# This one is tricky and may run better with adjustments to the -j parameter.
if [[ $CMD =~ "--recstart" ]]; then
  CMD="$(srun_parallel "$CMD")"

# crb-blast spawns parallel jobs itself, so run in a single srun.
# TODO would this run faster on savio than savio2_htc with more cores?
elif [[ $CMD == "crb-blast"* ]]; then
  CMD="$(srun_crb "$CMD")"

# These are quick commands that may be better to run locally depending on the
# queue. Check `squeue` and remove any that are piling up. Some that seem
# trivial like `cat` and `cut` can be IO-bound when many run at once on the
# same machine.
elif [[ $CMD == md5sum*      ||
        $CMD == makeblastdb* ||
        $CMD == blastdbget*  ||
        $CMD == cut*         ||
        $CMD == cat*         ||
        $CMD =~ ".py"        ||
        $CMD =~ ".R"         ]]; then
  CMD="$(srun_quick "$CMD")"

fi

# Run the finished command
# TODO proper logfile
echo "$CMD"  >> /tmp/wrapper.log
eval "$CMD" 2>> /tmp/wrapper.log
