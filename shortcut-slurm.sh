#!/usr/bin/env bash

# This is the top-level script for running ShortCut as a SLURM job. It
# reserves time + nodes + cpus, then calls shortcut when they're available.
# ShortCut calls srun for each "big" command as it goes, staying within the
# node limit set here. For now, each command uses all CPUs on its node.

# Example command:
# ../shortcut-slurm.sh 6 12 12:00:00 --script $HOME/shortcut/cuts/green.cut --debug

# TODO would it be more efficient to ask for each srun allocation outside salloc as we go?
# TODO try an interactive job next!

USAGE="usage: shortcut-slurm.sh <nodes> <cpus> <hh:mm:ss> [shortcut args]"
[[ $# -ge 4 ]] || (echo "$USAGE" && exit 1)

NUMNODES="$1"; shift
NUMCPUS="$1" ; shift
MAXTIME="$1" ; shift

SRUNCMD="$HOME/shortcut/shortcut-srun.sh"
SHORTCUT="$HOME/shortcut/result/bin/shortcut"
SHORTCUTCMD="$SHORTCUT --clusterlimit=$NUMNODES --clusterscript=$SRUNCMD $@"
SLURMARGS="--account=co_rosalind --partition=savio2_htc --qos=rosalind_htc2_normal"
SLURMARGS="$SLURMARGS --nodes=$(($NUMNODES + 1)) --cpus-per-task=$NUMCPUS --time=$MAXTIME"

# echo "SHORTCUTCMD: $SHORTCUTCMD"
salloc $SLURMARGS $SHORTCUTCMD
