#!/usr/bin/env bash

#SBATCH --job-name=greencut
#SBATCH --output=/global/home/users/jefdaj/shortcut/greencut_htc2.log
#SBATCH --time=12:00:00
#SBATCH --account=co_rosalind
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=12
#SBATCH --partition=savio2_htc
#SBATCH --qos=rosalind_htc2_normal

source $HOME/.bashrc
cd $HOME/shortcut/genomes
shortcut --script ../cuts/green.cut --debug
