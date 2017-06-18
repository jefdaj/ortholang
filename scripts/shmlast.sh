#!/usr/bin/env bash

# TODO switch to writing this in Python?
# args: script name, tmpdir, return path, function name, arg paths

export TMPDIR="$1"; shift
RTNPATH="$1"; shift
FNNAME="$1" ; shift

mkdir -p "$TMPDIR"

blast_rbh() {
	echo "blast_rbh $@"
}

blast_crbh() {
	echo "blast_crbh $@"
}

case "$FNNAME" in
  rbh ) blast_rbh  $@ ;;
  crbh) blast_crbh $@ ;;
  *) echo "invalid argument '$1'"; exit 1;;
esac
