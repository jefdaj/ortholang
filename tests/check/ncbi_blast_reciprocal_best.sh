# TODO is it actually always supposed to be 203?
# The number of hits varies pretty wildly, but there should at least be a few!
output="$(cat "${1}/output.txt")"
minhits=3
[[ $output -ge $minhits ]] || (echo -n "less than $minhits hits: $output" && exit 1)
