#!/usr/bin/env bash

# Tests that wrapping system calls doesn't break anything.

# TODO should this be the default when no --wrapper is specified?
# TODO set logfile from ortholang conig
# TODO rename to reflect that this will do the default pipe redirecting
# TODO remove all wrappedCmdOut calls before using this version

OUTPATH="$2" # TODO is this right?
STDOUTPATH="${OUTPATH}.out"
STDERRPATH="${OUTPATH}.err"

run() {
 echo "$@"  >> $TMPDIR/wrapper.log
 eval "$@" >> "$STDOUTPATH" 2>> "$STDERRPATH"
}

run "$@"
