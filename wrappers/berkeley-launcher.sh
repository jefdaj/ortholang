#!/bin/bash

# The other half of the mksrun setup. This one just launches all the scripts
# from outside the container. This is what you would run via sbatch to schedule
# a chunk of resources for all the commands at once.

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
