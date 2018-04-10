#!/bin/bash

# TODO ah is the group QoS limit 100? And I'm running 96 of them lol

# Wrapper for the Berkeley Computational Genomics Resource Laboratory (CGRL).
# It should work with minor changes on any system that uses the SLURM scheduler
# though if you adjust the SRUN command to match your system requirements.

# Note that you also need to set TMPDIR to something shared across machines
# before invoking shortcut. For example I use:
# export TMPDIR=/global/scratch/jefdaj/shortcut-test
# Otherwise you'll get lots of possibly-confusing srun errors!

# TODO set logfile from shortcut config

# Common options for all srun commands
SRUN="srun --account=co_rosalind --partition=savio2_htc --qos=rosalind_htc2_normal"
SRUN="$SRUN --chdir $(pwd) --quiet"
JOBSFILE="/tmp/jobs.txt" # use main tmpdir to share between instances

kill_jobs() {
  # In case of errors, make sure not to leave zombie srun jobs.
  # TODO kill only jobs from this process, in case multiple cuts running
  scancel -u $(whoami) --ctld --full
  finish_jobs 9999 # set jobs to 0
}

start_jobs() {
  # Try to add jobs to the jobs file, and fail if that exceeds the max.
  nnewjobs="$1"
  nmaxjobs=99 # TODO how many are typically going before errors start?
  (
    flock -x 13
    ncurjobs=$(cat "$JOBSFILE")
    [[ -z $ncurjobs ]] && ncurjobs=0
    ntotjobs=$(( $ncurjobs + $nnewjobs ))
    if [[ $ntotjobs -le $nmaxjobs ]]; then
      echo "$ntotjobs" > "$JOBSFILE"
      # sync # TODO remove?
    else
      return 1
    fi
  ) 13>"${JOBSFILE}.lock"
}

finish_jobs() {
  # Remove jobs from the jobs file. Should always succeed.
  ndonejobs="$1"
  (
    flock -x 13
    ncurjobs="$(cat "$JOBSFILE")"
    ntotjobs="$(( $ncurjobs - $ndonejobs ))"
    [[ $ntotjobs < 0 ]] && ntotjobs=0
    echo "$ntotjobs" > "$JOBSFILE"
    # sync # TODO remove?
  ) 13>"${JOBSFILE}.lock"
}

run() {
  # Beyond ~150-200 jobs at once, random SLURM bugs seem to surface which is a
  # bad time for everyone involved! This is super hacky but helps prevent that.
  njobs="$1"; shift
  cmd="$@"
  while true; do
    start_jobs $njobs && break || sleep 5
  done
  echo "$cmd" >> /tmp/wrapper.log
  eval "trap kill_jobs INT ERR; $cmd && finish_jobs $njobs" 2>> /tmp/wrapper.log
}

srun_crb() {
  srun="$SRUN --cpus-per-task=7 --nodes=1-1 --ntasks=1 --mem=50G --time=99:00:00"
  run 1 "$srun $@"
}

srun_psiblast() {
  # TODO any advantage of 7 vs all 8 cores?
  # TODO how much memory is realistically needed?
  # TODO how much wall time is realistically needed?
  srun="$SRUN --cpus-per-task=7 --nodes=1-1 --ntasks=1 --mem=50G --time=99:00:00"
  run 1 "$srun $@"
}

srun_small() {
  srun="$SRUN --cpus-per-task=1 --nodes=1-1 --ntasks=1 --time=00:10:00"
  run 1 "$srun $@"
}

srun_split() {
  srun="$SRUN --cpus-per-task=1 --nodes=1-1 --ntasks=1 --time=00:30:00"
  run 1 "$srun $@"
}

srun_parallel() {
  # monkey-patches a parallel call to run its individual commands via slurm
  # note that it's brittle and only works on shortcut-generated blast commands
  #sleep $(shuf -i 0-300 -n1) # prevent all trying to limit_total_jobs at once
  cmd="$@"
  before="$(echo "$cmd" | cut -d' ' -f-12)" # ... --pipe
  after="$(echo "$cmd" | cut -d' ' -f13-)"  # '*blast* ...

  # Using -N1 here says to run each record (sequence) separately. That slows it
  # down significantly, but on the rare occasion a sequence has an error like
  # an invalid amino acid, only that one will be exluded from the hits.
  # Instead, you might want to watch the output for errors, then fix them and
  # re-run any affected genomes.
  # TODO can/should I make it fail if one task fails?
  #      (possibly fixed in newer blast+ than 2.2.29?)
  # pargs="-j50 -N1 --delay 0.2" # additional parallel args
  pargs="-j20 --block-size 50k --delay 0.2" # additional parallel args

  # TODO --exclusive?
  # TODO any chance one will take more than 10 min?
  srun="$SRUN --cpus-per-task=1 --nodes=1-1 --ntasks=1 --time=00:30:00"
  cmd="${before} ${pargs} ${srun} ${after}"
  run 20 "$cmd"
}


# Make parallel blast run individual commands via srun.
# This one is tricky and may run better with adjustments to the -j parameter.
if [[ $@ =~ "--recstart" ]]; then
  srun_parallel "$@"

# psiblast is already done per gene, and can use threads on one node
# TODO would this run faster on savio than savio2_htc with more cores?
elif [[ $1 == *"psiblast"* ]]; then
  srun_psiblast "$@"

# crb-blast spawns parallel jobs itself, so run in a single srun.
# TODO would this run faster on savio than savio2_htc with more cores?
elif [[ $@ == "crb-blast"* ]]; then
  srun_crb "$@"

elif [[ $@ =~ "split_fasta.py" ]]; then
  srun_split "$@"

# These are quick commands that may be better to run locally depending on the
# queue. Check `squeue` and remove any that are piling up. Some that seem
# trivial like `cat` and `cut` can be IO-bound when many run at once on the
# same machine.
elif [[ $@ == md5sum*      ||
        $@ == makeblastdb* ||
        $@ == blastdbget*  ||
        $@ == cut*         ||
        $@ == cat*         ||
        $@ =~ ".py"        ||
        $@ =~ ".R"         ]]; then
  srun_small "$@"

# If nothing else matches run the command locally
else
  run 0 "$@"

fi
