#!/usr/bin/env bash

# TODO why is setting it here different from inside singularity?
export LC_ALL="en_US.UTF-8"

condo=/clusterfs/rosalind/users
scratch=/global/scratch
image=${condo}/jefdaj/shortcut.img
# shortcutcmd="shortcut $@"
shortcutcmd="bash $@"

# Annoyingly, these also have to go in singularity.nix before building
# $condo, /etc/hosts, and /etc/localtime are mounted automatically on berkeley hpc
# bindcmds=""
# for bindpoint in ${condo} /bin/flock /bin/srun /bin/scancel /bin/squeue; do
# 	bindcmds="$bindcmds -B ${bindpoint}:${bindpoint}"
# done
bindcmds="-B ${condo} -B /etc/slurm"

# cmd="singularity run $bindcmds $image"
cmd="singularity exec $bindcmds $image $shortcutcmd"
echo "$cmd" && eval "$cmd"
