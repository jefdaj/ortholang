#!/usr/bin/env bash

# storepaths="$(nix-build --dry-run 2>&1 | egrep '^\s*\/nix\/store')"
# n_storepaths="$(echo "$storepaths" | wc -l)"
# echo $storepaths
# echo "trying to build $n_storepaths packages in $n_seconds seconds..."

n_seconds=60
timeout $n_seconds nix-build -j$(nproc) --no-build-output 2>&1 | tee build.log

attempted="$(egrep "^building '.*'...$" build.log | cut -d"'" -f2 | sed 's/\.drv//g')"
for p in $attempted; do
  nix copy --to file://$HOME/nix.store $p || continue
done
# finished="$(for p in $storepaths; do [[ -a $p ]] && echo "$p"; done)"
# n_finished="$(echo "$finished" | wc -l)"
# echo "caching the $n_finished packages that finished for next time..."
# nix copy --to file://$HOME/nix.store $finished
