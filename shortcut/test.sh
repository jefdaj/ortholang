#!/usr/bin/env bash

# Runs Hspec with all the tests. Optionally takes one or more keywords and
# runs only the tests with matching descriptions.

keywords=""
for arg in $@; do
  keywords="$keywords -m $arg"
done

# TODO before committing, do with --qc-max-success=999 ?
stack test --file-watch --test-arguments="$keywords"
