#!/usr/bin/env bash

# echo "running outer wrapper..."

# This watches for comands and `srun`s them using singularity.
# TODO set matching TMPDIR in both
# TODO complicated logic here for which comands get how much time etc?

condo="/clusterfs/rosalind/users"
scratch=/global/scratch
cmddir="${scratch}/jefdaj/srun-commands"
image="${scratch}/jefdaj/shortcut.img"
srun_settings="--account=co_rosalind --partition=savio2_htc --time=00:01:00 --nodes=1 --ntasks-per-node=1"
# srun_settings="--account=co_rosalind --partition=savio --time=00:01:00 --nodes=1 --ntasks-per-node=1"

mkdir -p "$cmddir"

srun_singularity() {
  script="${1}"
  base="${1/.sh/}"
  logpath="${base}.log"
  lockpath="${base}.lock"
  outpath="${base}.out"
  exitpath="${base}.exit"
  # args="$(cat "$script")"
  name="$(head -n3 "$script" | tail -n1 | awk '{print $1}')"
  cmd="srun $srun_settings --job-name "$name" singularity exec -B ${condo} ${image} $script"
  echo "$cmd" # >> ${cmddir}/wrapper.log
  eval "$cmd" > "$outpath"
  echo $? > "$exitpath" # TODO does this work if eval fails?
  # sync
  # rm -f "$script" "$lockpath"
}

# cd "${cmddir}"
while sleep 0.1; do
  ls "${cmddir}"/*.sh 2>/dev/null | while read script; do
    lockpath="${script/.sh/.lock}"
    # if [[ ! -a "$lockpath" ]]; then
    ( flock -x 201 || continue
      srun_singularity "$script" &
    ) 201>"$lockpath"
    # fi
  done
done
