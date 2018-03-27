#!/usr/bin/env bash

# Tests that wrapping system calls doesn't break anything.

# TODO should this be the default when no --wrapper is specified?
# TODO set logfile from shortcut conig

run() {
 echo "$@"  >> /tmp/wrapper.log
 eval "$@" 2>> /tmp/wrapper.log
}

run "$@"
