#!/usr/bin/env bash

export build_cmd="nix-build -j$(nproc) --quiet dependencies.nix -A travisBuilds --keep-going"
storepaths="$($build_cmd --dry-run | grep '\/nix\/store')"
echo "these store paths will be attempted:"
echo $storepaths

timeout 120 $build_cmd

builtstorepaths="$(for p in $storepaths; do [[ -d $p ]] && echo $p; done | sed 's/\.drv//g')"
echo "these store paths were built:"
echo $builtstorepaths

echo "caching them for next time..."
for p in $builtstorepaths; do
  nix copy --to file://$HOME/nix.store $p
done
