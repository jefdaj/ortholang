#!/usr/bin/env bash

# Like the srun one, except for running on my 4-core laptop.
# (There's not a lot you can do besides setting it to use 4 threads)

# print a message if log level is set (from within ShortCut or by the user)
# TODO send this to a file rather than stdout?
log() { [[ -z $SHORTCUT_LOGLEVEL ]] || echo "[$(date '+%Y-%m-%d %H:%M:%S')] $@"; }

case "$(basename "$1")" in
	crb-blast) CMD="$@ --verbose --split --threads=4"; log "$CMD";;
	cat|ln) CMD="$@";; # TODO does anything echoed here get sent to the output file??
	*) CMD="$@"; log "$CMD";; # run the command as-is in this shell
esac

$CMD
