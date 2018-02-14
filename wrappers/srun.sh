#!/usr/bin/env bash

# TODO It would probably work better to have an overall salloc script run shortcut right?
#      That way you would know the available resources to set here.
#      Even if it seems more complicated that's probably worth it for time saved.

# TODO any point controlling number of threads here? or is that crb-blast specific?

# Don't forget to set this before running.
# TODO test out which values work best (may vary by command)
CPUS_PER_TASK=4
TIME_PER_TASK=01:00:00

# This is a wrapper script for running individual commands on other nodes.
# It's meant to be called from ShortCut with all the same arguments that make
# up a regular shell command. It runs the command using srun, then waits for a
# result before exiting. If needed, it can alter the command before running
# it, for example to set the number of threads used. srun inherits options
# from the outer salloc script but they can be overridden here. Certain quick
# commands like `ln` are just run normally.

SRUN="srun --account=co_rosalind --partition=savio2_htc --qos=rosalind_htc2_normal"
SRUN="$SRUN --chdir $(pwd) --nodes=1-1 --ntasks=1"

# unset LIBRARY_PATH
# unset LD_LIBRARY_PATH

# print a message if log level is set (from within ShortCut or by the user)
# TODO send this to a file rather than stdout?
log() { [[ -z $SHORTCUT_LOGLEVEL ]] || echo "[$(date '+%Y-%m-%d %H:%M:%S')] $@"; }

# put any changes needed to optimize specific commands here
case "$(basename "$1")" in
	crb-blast) CMD="$SRUN --cpus-per-task=$CPUS_PER_TASK --time=$TIME_PER_TASK $@ --verbose --split --threads=12"; log "$CMD";;
	parallelblast.py) CMD="$SRUN --cpus-per-task=$CPUS_PER_TASK --time=$TIME_PER_TASK $@"; log "$CMD";;
	blastn|blastp|blastx|tblastn|tblastx|megablast) CMD="$SRUN --cpus-per-task=$CPUS_PER_TASK --time=$TIME_PER_TASK $@"; log "$CMD";;
	cat|ln) CMD="$@";; # TODO does anything echoed here get sent to the output file??
	*) CMD="$@"; log "$CMD";; # run the command as-is in this shell
esac

$CMD
