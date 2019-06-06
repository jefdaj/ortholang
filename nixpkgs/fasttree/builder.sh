#!/usr/bin/env bash

source $stdenv/setup
mkdir -p $out/bin

# TODO: -DUSE_DOUBLE to improve precision for small branch lengths?

gcc -O3 -finline-functions -funroll-loops -Wall \
  -o $out/bin/FastTree $src -lm
gcc -DOPENMP -fopenmp -O3 -finline-functions -funroll-loops -Wall \
  -o $out/bin/FastTreeMP $src -lm
