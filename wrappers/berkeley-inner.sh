#!/bin/bash

# echo "$@"
# echo "running inner wrapper..."

# Instead of running commands directly, this writes them to a queue.
# There will be a second daemon process to `srun` everything that shows
# up in the queue.

scratch=/global/scratch
cmddir="${scratch}/jefdaj/srun-commands" # TODO where should this actually go?

hashpath="${cmddir}/$(echo "$@" | md5sum | awk '{print $1}')"
script="${hashpath}.sh"
exitpath="${hashpath}.exit"
# outpath="${cmddir}/${hashpath}.out"

function cleanup() {
  rm -f "$hashpath".{exit,out}
}
trap cleanup EXIT

# write the command to a file which will be picked up by the other script
# TODO fun/terrible idea: have the script delete itself?
echo "${script}: $@"  >> "${cmddir}/wrapper.log"
[[ -a "$exitpath" ]] && exit || cat << EOF > "$script"
#!/bin/bash
$@
EOF
chmod +x "$script"

# then wait for it to finish running
while sleep 0.1; do [[ -a "$exitpath" ]] && break; done

# delete files and exit, passing on the code
exitcode=$(cat "$exitpath")
echo "${exitpath}: $exitcode" >> "${cmddir}/wrapper.log"
# sync
# cat "$outpath" >> "${cmddir}/wrapper.log"
cat "$outpath"
echo "finished running inner wrapper"
# rm -rf "$argspath" "$exitpath" "$outpath" "$lockpath"
# rm -rf "$argspath"
# rm -rf "${hashpath}".*
exit $exitcode
