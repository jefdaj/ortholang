#!/bin/bash

# Tests that wrapping system calls doesn't break anything.

# TODO should this be the default when no --wrapper is specified?
# TODO set logfile from shortcut conig

export CONDO=/clusterfs/rosalind/users
export TMPDIR=/clusterfs/rosalind/users/jefdaj/shortcut/shortcut-tmpdir


MPI="mpirun -np 1 --mca plm_rsh_agent /global/home/users/jefdaj/shortcut/wrappers/openmpi2.sh"
# MPI="mpirun -np 1"

# fails with something about unable to locate a usable srun command in PATH:
# MPI="mpirun -np 1 exec ${CONDO}/jefdaj/shortcut.img"

run() {
 echo "$@"  >> $TMPDIR/wrapper.log
 eval "$@" 2>> $TMPDIR/wrapper.log
}

run "$MPI $@"
