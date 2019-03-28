#!/bin/bash

# Tests that wrapping system calls doesn't break anything.

# TODO should this be the default when no --wrapper is specified?
# TODO set logfile from shortcut conig

export CONDO=/clusterfs/rosalind/users
export TMPDIR=/clusterfs/rosalind/users/jefdaj/shortcut/shortcut-tmpdir


MPI="mpirun --mca plm_rsh_agent '/global/home/users/jefdaj/shortcut/wrappers/openmpi2.sh' -np 1"

run() {
 echo "$@"  >> $TMPDIR/wrapper.log
 eval "$@" 2>> $TMPDIR/wrapper.log
}

run "$MPI $@"
