#!/usr/bin/env bash

export build_cmd="nix-build -j$(nproc) --quiet dependencies.nix -A travisBuilds"
storepaths="$($build_cmd --dry-run | grep store | xargs echo)"
timeout 120 $build_cmd
builtstorepaths="$(for p in $storepaths; do [[ -d $p ]] && echo $p; done | xargs echo)"
nix copy --to=$HOME/nix.store $builtstorepaths
