#!/usr/bin/env bash

export build_cmd="nix-build -j$(nproc) --quiet dependencies.nix -A travisBuilds"
storepaths="$($build_cmd --dry-run | grep store | xargs echo)"
timeout 120 $build_cmd
builtstorepaths="$(for p in $storepaths; do [[ -d $p ]] && echo $p; done)"
echo "these store paths were built:"
echo $builtstorepaths
echo "caching them for next time..."
for p in $builtstorepaths; do
  nix copy --to file://$HOME/nix.store $p
done
