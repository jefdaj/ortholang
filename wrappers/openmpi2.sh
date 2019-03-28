#!/usr/bin/env bash
export CONDO=/clusterfs/rosalind/users
singularity exec -B $CONDO ${CONDO}/jefdaj/shortcut.img
