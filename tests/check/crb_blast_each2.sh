# Tests that crb_blast tmpfiles are deduplicated properly.
# TODO once deduping works, set this to 1
ndirs=$(ls "${1}/cache/crb_blast" | wc -l)
[[ $ndirs -eq 2 ]] || (echo -n "wrong number of crb_blast cache dirs: $ndirs" && exit 1)
