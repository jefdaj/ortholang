#!/bin/bash

# Third(?) attempt at a wrapper script for the Berkeley HPC environment that
# works with both a read-only Singularity image and the SLURM scheduler. The
# idea is to write a fully self-contained script for each command to a shared
# tmpdir, and leave a separate "script runner" thread going that launches
# anything in that tmpdir from outside the image.
#
# This script is called with the regular command as arguments. For example:
#
# ./wrappers/berkeley-mksrun.sh md5sum /path/to/file.faa
#
# It writes a fully self-contained srun script to the shared tmpdir, which when
# launched will queue that command to run via the singularity container.

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
srun_args="--chdir $(pwd) --quiet $srun_args"
srun_name=$(echo "$1" | awk '{print $1}')
srun_cmd="srun $srun_args --job-name "$srun_name" singularity --silent exec -B ${condo} ${image} $1"

# write the srun script, which includes some extra machinery to ensure only one
# instance runs and to return stdout + stderr to this script when done
cat << EOF > "$scriptpath"
#!/bin/bash
( flock -x 200 || exit 1
echo "$srun_cmd" >> ${srundir}/wrapper.log
set -E
$srun_cmd &> "$outpath"
echo "$?" > "$exitpath"
) 200>$lockpath
EOF
chmod +x "$scriptpath"

# make sure to remove all the tmpfiles when done, unless there was an error
function cleanup() {
  exitcode=$(cat "$exitpath")
  if [[ $exitcode == 0 ]]; then
    rm -f "$scriptpath" "$outpath" "$exitpath"
  fi
  rm -f "$lockpath"
}
trap cleanup EXIT

# then wait for it to finish running
# TODO any way to check that the launcher script is running first?
while sleep 0.1; do [[ -a "$exitpath" ]] && break; done

# delete files and exit, passing on the code
export exitcode=$(cat "$exitpath")
cat "$outpath"
echo "dummy output" # TODO wtf, why is this required??
exit $exitcode
