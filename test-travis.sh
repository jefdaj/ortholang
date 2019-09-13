#!/usr/bin/env bash

nix-build dependencies.nix -f travisBuilds
./test.sh
