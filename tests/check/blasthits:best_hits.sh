# The number of hits varies pretty wildly, but there should at least be a few!
output="$(cat "${1}/output.txt" | cut -d'.' -f1)"
minhits=10
[[ $output -ge $minhits ]] || (echo -n "less than $minhits hits: $output" && exit 1)
