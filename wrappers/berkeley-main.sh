#!/usr/bin/env bash

# TODO do an sbatch here just to get it off the login node,
#      and get it set up so it has one big chunk reserved beforehand

singularity exec -B /clusterfs/rosalind/users $CONDO/shortcut.img shortcut --wrapper $(pwd)/srun-wrapper-inner.sh --script tests/scripts/mmseqs_search.cut --interactive
