#!/bin/bash

# Part of a two-part wrapper script for Berkeley's HPC environment that hacks
# around the inability to use SLURM from within a Singularity image. This
# script is called with no arguments. It simply launches any .sh file that
# appears in the shared tmpdir.
#
# This is the one you want to run with sbatch to reserve a chunk of resources
# for all commands. If you don't, the scripts will each wait for resources
# individually.

# TODO default sbatch args here

condo="/clusterfs/rosalind/users"
scratch=/global/scratch/jefdaj
# image="${scratch}/ortholang.img"
srundir="${scratch}/srun-commands"

mkdir -p "$srundir"

# TODO decide time, nodes etc. per command? or use a large default and sbatch a block of time first
srun_name=$(echo "$1" | awk '{print $1}')
srun_cmd="srun --chdir $(pwd)"

while sleep 1; do
  ls "${srundir}"/*.sh 2>/dev/null | while read script; do
    lockpath="${script/.sh/.lock}"
    [[ -a "$lockpath" ]] && continue
    cmd="$srun_cmd $script"
    # echo "$cmd"
    $cmd &
  done
done
