# Tests that crb_blast tmpfiles are deduplicated properly.
ndirs=$(ls "${1}/cache/crb_blast" | wc -l)
[[ $ndirs -eq 1 ]] || (echo -n "wrong number of crb_blast cache dirs: $ndirs" && exit 1)
