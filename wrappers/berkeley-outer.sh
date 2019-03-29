#!/usr/bin/env bash

# echo "running outer wrapper..."

# This watches for comands and `srun`s them using singularity.
# TODO set matching TMPDIR in both
# TODO complicated logic here for which comands get how much time etc?

condo="/clusterfs/rosalind/users"
cmddir="${condo}/jefdaj/srun-commands"
image="${condo}/jefdaj/shortcut.img"
# srun_settings="--account=co_rosalind --partition=savio2_htc --time=00:01:00 --nodes=1 --ntasks-per-node=1"
srun_settings="--account=co_rosalind --partition=savio --time=00:01:00 --nodes=1 --ntasks-per-node=1"

mkdir -p "$cmddir"

srun_singularity() {
  argspath="${1}"
  hashpath="${1/.args/}"
  logpath="${hashpath}.log"
  lockpath="${hashpath}.lock"
  outpath="${hashpath}.out"
  exitpath="${hashpath}.exit"
  args="$(cat "$argspath")"
  name="$(echo $args | awk '{print $1}')"
  cmd="srun $srun_settings --job-name "$name" singularity exec -B ${condo} ${image} $args"
  echo "$cmd" # >> ${cmddir}/wrapper.log
  $cmd > "$outpath"
  echo $? > "$exitpath" # TODO does this work if eval fails?
  sync
  # rm -f "$argspath" "$lockpath"
}

# cd "${cmddir}"
while sleep 0.1; do
  for argspath in $(ls "${cmddir}"/*.args 2>/dev/null); do
    lockpath="${argspath/.args/.lock}"
    if [[ ! -a "$lockpath" ]]; then
      # echo "$argspath"
      touch "$lockpath" && srun_singularity "$argspath" &
    fi
  done
done
