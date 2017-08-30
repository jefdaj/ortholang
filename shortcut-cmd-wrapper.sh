#!/usr/bin/env bash

# This is a wrapper script for running individual commands on other nodes.
# It's meant to be called from ShortCut with all the same arguments that make
# up a regular shell command. It runs the command using srun, then waits for a
# result before exiting. If needed, it can alter the command before running
# it, for example to set the number of threads used. srun inherits options
# from the outer salloc script but they can be overridden here.

SRUN="srun --account=co_rosalind --partition=savio2_htc --qos=rosalind_htc2_normal"
SRUN="$SRUN --chdir $(pwd) --nodes=1-1 --ntasks=1"

# unset LIBRARY_PATH
# unset LD_LIBRARY_PATH

# put any changes needed to optimize specific commands here
# TODO set an environment variable (SOMETHING_DEBUG) that controls whether to echo the cmd
case "$(basename "$1")" in
	crb-blast) CMD="$SRUN --cpus-per-task=12 --time=99:00:00 $@ --verbose --split --threads=12"; echo "$CMD";;
	parallelblast|parallelblast.py) CMD="$SRUN --cpus-per-task=12 --time=99:00:00 $@"; echo "$CMD";;
	blastn|blastp|blastx|tblastn|tblastx) CMD="$SRUN --cpus-per-task=1 --time=99:00:00 $@"; echo "$CMD";;
	cat|ln) CMD="$@";; # TODO does anything echoed here get sent to the output file??
	*) CMD="$@"; echo "$CMD";; # run the command as-is in this shell
esac

$CMD
