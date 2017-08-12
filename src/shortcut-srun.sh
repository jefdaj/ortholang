#!/usr/bin/env bash

# This is a wrapper script for running individual commands on other nodes.
# It's meant to be called from ShortCut with all the same arguments that make
# up a regular shell command. It runs the command using srun, then waits for a
# result before exiting. If needed, it can alter the command before running
# it, for example to set the number of threads used. srun inherits options
# from the outer salloc script but they can be overridden here.

USAGE="usage: shortcut-srun.sh <shell command with whatever args>"
[[ $# -ge 1 ]] || (echo "$USAGE" && exit 1)

# put any changes needed to optimize specific commands here
CMD="$@"
PROGNAME="$(basename "$1")"
case "$PROGNAME" in
  crb-blast) CMD="$CMD --split --threads=\$SLURM_CPUS_ON_NODE" ;;
	# TODO need a * case here?
esac

# TODO does this wait automatically?
# TODO does it need an output file?
# TODO how about a unique name?
SRUNARGS="--nodes=1 --name=$PROGNAME"
srun $SRUNARGS "$CMD"
