#!/usr/bin/env bash

set -e

DBPATH="$1"
diamond dbinfo --db "$DBPATH"
