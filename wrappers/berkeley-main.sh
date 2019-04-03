#!/bin/bash

#old --job-name=shortcut
#old --output=/global/home/users/jefdaj/shortcut/shortcut.log
#old --time=03:00:00
#old --account=co_rosalind
#old --partition=savio
#old --qos=rosalind_savio_normal
#old --nodes=1

export LC_ALL="en_UT.UTF-8"

DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" >/dev/null 2>&1 && pwd )"

# launch the outer wrapper, which watches for new scripts and runs them with srun + singularity
# ${DIR}/berkeley-outer.sh &
# pid=$!

# cleanup() {
#   (
#   kill $pid
#   scancel -u jefdaj
#   rm -f /clusterfs/rosalind/users/jefdaj/srun-commands/*.{lock,exit,out,sh}
#   ) &> /dev/null
#   exit 0
# }
# trap cleanup EXIT

# launch shortcut, which will run its system commands via the inner script,
# which will write them to scripts for the outer script to execute
singularity --silent exec \
  -B /clusterfs/rosalind/users \
  /clusterfs/rosalind/users/jefdaj/shortcut.img \
  shortcut $@
