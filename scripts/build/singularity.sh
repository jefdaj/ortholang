#!/usr/bin/env bash

# TODO why is setting it here different from inside singularity?
export LC_ALL="en_US.UTF-8"

condo=/clusterfs/rosalind/users
scratch=/global/scratch
image=${scratch}/jefdaj/ortholang.img
ortholangcmd="ortholang $@ --wrapper $(pwd)/wrappers/openmpi.sh"
# ortholangcmd="bash $@"

# Annoyingly, these also have to go in singularity.nix before building
# $condo, /etc/hosts, and /etc/localtime are mounted automatically on berkeley hpc
# bindcmds=""
# for bindpoint in ${condo} /bin/flock /bin/srun /bin/scancel /bin/squeue; do
# 	bindcmds="$bindcmds -B ${bindpoint}:${bindpoint}"
# done
# bindcmds="-B ${condo} -B /etc/slurm"
bindcmds="-B ${condo}"

# cmd="singularity run $bindcmds $image"
cmd1="mpirun -n1 singularity exec $bindcmds $image $ortholangcmd"
# echo "$cmd1"

# cmd2="srun --job-name=ortholang --output=/global/home/users/jefdaj/ortholang/singularity.log --time=00:01:00 --account=co_rosalind --partition=savio2_htc --qos=rosalind_htc2_normal --nodes=2 --ntasks-per-node=1 $cmd1"
# cmd2="srun --job-name=ortholang --output=/global/home/users/jefdaj/ortholang/singularity.log --time=00:01:00 --account=co_rosalind --partition=savio2_htc --mpi=pmi2 --nodes=1 --ntasks-per-node=1 $cmd1"
cmd2="srun --job-name=ortholang --output=/global/home/users/jefdaj/ortholang/singularity.log --time=00:01:00 --account=co_rosalind --partition=savio2_htc --mpi=pmi2 --nodes=1 --ntasks-per-node=1 $cmd1"
echo "$cmd2"

eval "$cmd2"
