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

scratch=/global/scratch/jefdaj
srundir="${scratch}/srun-commands"

mkdir -p "$srundir"

while sleep 0.1; do
  ls "${srundir}"/*.sh 2>/dev/null | while read script; do
    lockpath="${script/.sh/.lock}"
    [[ -a "$lockpath" ]] && continue
    echo "launching $script"
    . "$script" &
  done
done
