#!/bin/bash

# TODO how to make sure stdout/stderr here go to the outpath instead of launcher terminal?

# TODO document that this writes shell scripts only now, and srun stuff goes in the launcher

# TODO hm, do i need to go back to putting all the srun stuff in the outer wrapper so i can run the script itself via singularity? that would get around any quoting issues

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
hashpath="${srundir}/$(echo "$1" | md5sum | awk '{print $1}')"
scriptpath="${hashpath}.sh"
lockpath="${hashpath}.lock"
outpath="${hashpath}.out"
exitpath="${hashpath}.exit"

# write the srun script, which includes some extra machinery to ensure only one
# instance runs and to return stdout + stderr to this script when done
# TODO did the && sync after srun_cmd line help?
cat << EOF > "$scriptpath"
#!/bin/bash
( flock -n -x 200 || exit 0
echo "$1" >> ${srundir}/wrapper.log
($1) &> "$outpath"
echo "\$?" > "$exitpath"
) 200>$lockpath
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
