#!/usr/bin/env bash

# this prevents accidentally including cruft in the upload
rm -rf .stack-work dist-newstyle

nix-store -qR --include-outputs $(nix-instantiate release.nix) | cachix push jefdaj
