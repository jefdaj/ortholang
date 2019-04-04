#!/bin/bash

# Part of a two-part wrapper script for Berkeley's HPC environment that hacks
# around the inability to use SLURM from within a Singularity image. This
# script is called by shortcut with the regular command as argument 1. For
# example:
#
# ./wrappers/berkeley-writer.sh 'md5sum /path/to/file.faa'
#
# It writes a self-contained script to a shared tmpdir, which when launched
# will queue that command to run via SLURM. At the same time, you should be
# running berkeley-launcher.sh in a separate terminal to run the script from
# *outside* the container as soon as it's written. This script will block until
# the srun script it wrote finishes running.
#
# This is the one you want to use to tweak srun_args based on the individual
# commands being run. Use berkeley-launcher.sh to reserve overall resources, or
# run that regularly (no sbatch) and each individual script will wait for its
# own resources.

condo="/clusterfs/rosalind/users"
scratch='/global/scratch/jefdaj'
srundir="${scratch}/srun-commands"
image="${scratch}/shortcut.img"
hashpath="${srundir}/$(echo "$@" | md5sum | awk '{print $1}')"
scriptpath="${hashpath}.sh"
lockpath="${hashpath}.lock"
outpath="${hashpath}.out"
exitpath="${hashpath}.exit"

# TODO decide time, nodes etc. per command? or use a large default and sbatch a block of time first
srun_args="--account=co_rosalind --partition=savio2_htc --time=00:01:00 --nodes=1 --ntasks-per-node=1"
srun_name=$(echo "$1" | awk '{print $1}')
srun_args="--chdir $(pwd) --quiet $srun_args --job-name $srun_name"
srun_cmd="srun $srun_args singularity --silent exec -B ${condo} ${image} $1"

# write the srun script, which includes some extra machinery to ensure only one
# instance runs and to return stdout + stderr to this script when done
# TODO did the && sync after srun_cmd line help?
cat << EOF > "$scriptpath"
#!/bin/bash
( flock -x 200 || exit 1
echo "$srun_cmd" >> ${srundir}/wrapper.log
$srun_cmd &> "$outpath"
echo "$?" > "$exitpath"
) 200>"$lockpath"
EOF
chmod +x "$scriptpath"

# wait for it to finish running
# then delete files and exit, passing on the code
# TODO any way to check that the launcher script is running first?
while sleep 0.1; do [[ -a "$exitpath" ]] && break; done
cat "$outpath"
exitcode=$(cat "$exitpath")
[[ $exitcode == 0 ]] && rm -f "$lockpath" "$scriptpath" "$exitpath" "$outpath"
exit $exitcode
