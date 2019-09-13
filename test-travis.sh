#!/usr/bin/env bash

build_cmd="nix-build -j$(nproc) --quiet dependencies.nix -A travisBuilds --keep-going"
storepaths="$($build_cmd --dry-run | grep '\/nix\/store')"
echo "these store paths will be attempted: $storepaths"

echo "building as many as possible..."
timeout 600 $build_cmd

builtstorepaths="$(for p in $storepaths; do [[ -a $p ]] && echo $p; done)"
echo "these store paths were built: $builtstorepaths"

echo "caching them for next time..."
for p in $builtstorepaths; do
  nix copy --to file://$HOME/nix.store $p
done
