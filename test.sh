#!/usr/bin/env bash

# Runs Hspec with all the tests. Optionally takes one or more keywords and
# runs only the tests with matching descriptions.
# TODO remove this once testing is well integrated in the main binary

keywords=""
for arg in $@; do
  keywords="$keywords -m $arg"
done

# TODO before committing, do with --qc-max-success=999 ?
TMPDIR=$(pwd)/../.tmp stack test \
  --test-arguments="$keywords" \
  --allow-different-user \
  --file-watch
