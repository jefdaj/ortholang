#!/bin/bash

# Wrapper for the Berkeley Computational Genomics Resource Laboratory (CGRL).
# It should work with minor changes on any system that uses the SLURM scheduler
# though if you adjust the SRUN variable below.

# Note that you also need to set TMPDIR to something shared across machines
# before invoking shortcut. For example I use:
# export TMPDIR=/global/scratch/jefdaj/shortcut-test
# Otherwise you'll get lots of srun errors "cannot chdir to /tmp/whatever..."

# TODO offload hashing with srun_single too if possible
# TODO control number of parallel jobs in srun_parallel rather than haskell!

# Common options for all srun commands
SRUN="srun --account=co_rosalind --partition=savio2_htc --qos=rosalind_htc2_normal"
SRUN="$SRUN --chdir $(pwd) --quiet"
# TODO --exclusive -N1 -n1?

srun_single() {
  # This is mostly for crb-blast so far
  cmd="$@"
  srun="$SRUN --cpus-per-task=4 --nodes=1-1 --ntasks=1 --mem=20G --time=99:00:00"
  cmd="$srun $cmd"
  echo "$cmd"
}

srun_parallel() {
  # monkey-patches a parallel call to run its individual commands via slurm
  # note that it's brittle and only works on shortcut-generated blast commands
  cmd="$@"
  before="$(echo "$cmd" | cut -d' ' -f-16)" # ... --pipe
  after="$(echo "$cmd" | cut -d' ' -f17-)"  # '*blast* ...
  srun="$SRUN --cpus-per-task=1 --nodes=1-1 --ntasks=1 --time=99:00:00"
  cmd="${before} $srun ${after}"
  echo "$cmd"
}

# If none of the special cases below match, this will run as-is.
CMD="$@"

if [[ $CMD =~ "--recstart" ]]; then
  # Make parallel blast run individual commands via srun
  CMD="$(srun_parallel "$CMD")"
elif [[ $CMD == "crb-blast"* ]]; then
  # crb-blast spawns parallel jobs itself, so run in a single srun
  CMD="$(srun_single "$CMD")"
fi

# Run the finished command
# TODO proper logfile
echo "$CMD"  >> /tmp/wrapper.log
eval "$CMD" 2>> /tmp/wrapper.log
