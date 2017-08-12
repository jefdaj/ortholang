#!/usr/bin/env bash

# This is a wrapper script for running individual commands on other nodes.
# It's meant to be called from ShortCut with all the same arguments that make
# up a regular shell command. It runs the command using srun, then waits for a
# result before exiting. If needed, it can alter the command before running
# it, for example to set the number of threads used. srun inherits options
# from the outer salloc script but they can be overridden here.

unset LIBRARY_PATH
unset LD_LIBRARY_PATH
USAGE="usage: shortcut-srun.sh <shell command with whatever args>"
[[ $# -ge 1 ]] || (echo "$USAGE" && exit 1)

WORKINGDIR="$1"; shift # to put srun code back in its proper working dir
BIN="$1"; shift # for adding args based on program name
ARGS="$@"

# put any changes needed to optimize specific commands here
case "$(basename "$BIN")" in
	crb-blast) ARGS="$ARGS --split --threads=$SLURM_CPUS_PER_TASK" ;;
esac

# TODO does this wait automatically?
# TODO does it need an output file?
LOGFILE="$HOME/shortcut/shortcut.log"
SRUNARGS="--chdir $WORKINGDIR --nodes=1-1 --ntasks=1"

# echo "ARGS: $ARGS"
# env | grep SLURM
# TODO add date to output?
echo "$BIN $ARGS"
srun $SRUNARGS $BIN $ARGS
