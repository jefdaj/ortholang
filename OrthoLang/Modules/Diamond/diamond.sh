#!/usr/bin/env bash

set -e

OPATH="$1"; shift
QPATH="$1"; shift
ESTR="$1"; shift
DBPATH="$1"; shift
DCMD="$@"

diamond $DCMD -q "$QPATH" -o "${OPATH}.tmp" -e "$ESTR" -d "$DBPATH" > "${OPATH}.out" 2> "${OPATH}.err" && mv "${OPATH}.tmp" "${OPATH}"

# this works around a bug where it would sometimes crash and leave a partially written table
# TODO remove when sure that's fixed
sync
last_line="$(tail -n1 "${OPATH}")"
n_tabs="$(echo "$last_line" | sed 's/[^\t]//g' | awk '{print length}')"
if [[ $n_tabs != 11 ]]; then
  echo "error! ${OPATH} is incomplete. last line has ${n_tabs} tabs:\n${last_line}"
	rm "${OPATH}" "${OPATH}.tmp" -f
	exit 1
fi
