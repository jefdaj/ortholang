#!/usr/bin/env bash

# Caches tmpfiles for the (working) test scripts.
# TODO separate scripts to download all databases

echo "building shortcut with stack..."
stack build
rm -rf tmpdir-cache

# in case of any locking errors, just repeat 3x for now
for n in {1..3}; do
  echo -n "filling cache (round $n)"
	# randomize order in case it matters
  ls tests/scripts/*.ol | shuf | while read f; do
    stack exec shortcut -- --tmpdir tmpdir-cache --script $f &>/dev/null && echo -n "." || echo "\nERROR: $f"
  done
	echo
done

echo 'removing non-cachable files...'
pushd tmpdir-cache
for ptn in \
	'vars' 'reps' 'proile.html' 'cache/load*' 'exprs/glob*' 'exprs/str' 'exprs/num' \
	'exprs/psiblast_pssm' 'exprs/psiblast_pssm_all' 'exprs/psiblast_train*' '*/venndiagram' '*/sonicparanoid*'; do
  rm -rf $ptn
done
popd

(echo "new cache:"; du -h tmpdir-cache | tail -n1; tree -L 2 tmpdir-cache) | less
