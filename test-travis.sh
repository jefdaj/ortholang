#!/usr/bin/env bash

# Runs test.sh for up to 30min, then backs up the Nix store.
# Use to prevent the free version of Travis CI from timing out on big builds.
# (It will still time out, but you can re-run the job until it finishes)

timeout $((30 * 60)) ./test.sh $@
