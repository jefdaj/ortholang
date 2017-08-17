#!/usr/bin/env bash

# This is a wrapper script for running individual commands on other nodes.
# It's meant to be called from ShortCut with all the same arguments that make
# up a regular shell command. It runs the command using srun, then waits for a
# result before exiting. If needed, it can alter the command before running
# it, for example to set the number of threads used. srun inherits options
# from the outer salloc script but they can be overridden here.

SRUN="srun --account=co_rosalind --partition=savio2_htc --qos=rosalind_htc2_normal"
SRUN="$SRUN --chdir $(pwd) --nodes=1-1 --ntasks=1"

# put any changes needed to optimize specific commands here
# TODO set an environment variable (SOMETHING_DEBUG) that controls whether to echo the cmd
case "$(basename "$1")" in
	crb-blast) CMD="$SRUN --cpus-per-task=12 --time=99:00:00 $@ --split --threads=12"; echo "$CMD";;
	ln) CMD="$@";;
	*) CMD="$@"; echo "$cmd";; # run the command as-is in this shell
esac

eval "$CMD"
