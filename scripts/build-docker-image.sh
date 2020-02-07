#!/usr/bin/env bash

# Usage ./dev-scripts/build-docker-image.sh
# Uncomment the last run_repl line for manual debugging.
# Command line arguments are passed to nix-build.

set -e

# Set to something with >7.5GB free for the nix-build step
export TMPDIR=/mnt/scratch

# leave blank if nothing special needed
export NIX_ARGS="$@"

build_docker_image() {
  nix-build docker.nix --out-link result-docker $NIX_ARGS
  docker load -i result-docker
}

run_docker_image() {
  latest=$(docker images | grep ortholang | head -n1 | awk '{print $2}')
  docker run $@ \
	  ortholang:$latest ortholang $ORTHOLANG_ARGS
}

run_tests() {
  export ORTHOLANG_ARGS='--test all'
  run_docker_image -t
}

run_repl() {
  export ORTHOLANG_ARGS='--interactive'
	run_docker_image -i -t \
    --mount type=bind,source="$TMPDIR",target="/tmpdir" \
    --mount type=bind,source="$PWD",target="/workdir"
}

build_docker_image 2>&1 | tee docker-build.log
run_tests 2>&1 | tee docker-test.log
# run_repl
