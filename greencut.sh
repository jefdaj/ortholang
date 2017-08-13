#!/usr/bin/env bash

#SBATCH --job-name=greencut
#SBATCH --output=/global/home/users/jefdaj/shortcut/greencut.log
#SBATCH --time=12:00:00
#SBATCH --account=co_rosalind
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=20
#SBATCH --partition=savio
#SBATCH --qos=rosalind_savio_normal

source $HOME/.bashrc
cd $HOME/shortcut/genomes
shortcut --script ../cuts/green.cut --debug
