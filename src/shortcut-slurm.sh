#!/usr/bin/env bash

# This is the top-level script for running ShortCut as a SLURM job. It
# reserves time + nodes + cpus, then calls shortcut when they're available.
# ShortCut calls srun for each "big" command as it goes, staying within the
# node limit set here. For now, each command uses all CPUs on its node.

USAGE="usage: shortcut-slurm.sh <nodes> <cpus> <hh:mm:ss> [shortcut args]"
[[ $# -ge 4 ]] || (echo "$USAGE" && exit 1)

LOGFILE="$HOME/shortcut/shortcut.log"
SRUNCMD="$HOME/shortcut/shortcut-srun.sh"
SHORTCUTCMD="shortcut --cluster-limit=$1 --cluster-script=$SRUNCMD ${@:4}"
SLURMARGS="--account=co_rosalind --partition=savio2_htc --qos=rosalind_htc_normal"
SLURMARGS="$SLURMARGS --nodes=$1 --cpus-per-node=$2 --time=$3 --output=$LOGFILE"

salloc $SLURMARGS "$SHORTCUTCMD"
