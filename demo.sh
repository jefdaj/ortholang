#!/usr/bin/env bash

# Simplest possible launcher for the poster demo script.
# Should be run inside nix-shell, or at least with python-docopt installed.
# Usage: demo.sh <cut script to demo>

SCRIPT="$1"
TMPSRC="/tmp/shortcut-demo-tmpsrc"
TMPDST="/tmp/shortcut-demo-tmpdst"
cd poster
# [[ -z "$@" ]] && SCRIPT="$(ls *.cut)" || SCRIPT="$@"

if [[ ! -d "$TMPSRC" ]]; then
    mkdir -p "$TMPSRC"
    echo "running script once to create tmpfiles..."
    # for cut in $SCRIPT; do
    cmd="shortcut --script '$SCRIPT' --tmpdir '$TMPSRC'"
    echo "$cmd" && eval "$cmd"
    # done
fi
# cmd="shortcut --script "$SCRIPT" --tmpdir /tmp/shortcut-demo-tmpsrc"
# echo "$cmd" && eval "$cmd"

# for cut in $SCRIPT; do
echo "launching the demo with $SCRIPT..."
./demo.py -s "$TMPSRC" -t "$TMPDST" -c "$SCRIPT"
# done
