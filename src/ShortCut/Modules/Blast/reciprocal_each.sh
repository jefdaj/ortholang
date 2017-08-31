#!/usr/bin/env bash

# Kludge to write the *blast*_rbh_each functions until I figure out a more
# elegant pattern.

# TODO haskell code will provide this script with:
#      - tmpdir (via Cwd?)
#      - final outpath :: [bht]
#      - single left hits table :: bht
#      - file listing rhits tables :: [bht]

# TODO file organization:
#      - each hit table: cache/reciprocal_each/<md5 lhits>/<md5 rhits>.bht
#      - final list of them: expr/<blast fn>_rbh_each/whatever.bht.list
#      - script runs in the cache dir
#      - if final file exists this won't run at all right?
#      - should check if each output bht exists before launching R script
#      - paths aren't reliable because they might be vars, so md5 everything?

# TODO once it works, parallelize? should be doable but maybe not needed?
# TODO need to add md5sum to nix?

# TODO rewrite to take into account that we need to "zip" two lists of tables

# OUTBHTLIST="$1"
# LHITSBHTLIST="$2"
# RHITSBHTLIST="$3"
# 
# echo "reciprocal_each.sh cwd: $(pwd)"
# echo "reciprocal_each.sh args: $@"
# echo "OUTBHTLIST  : $OUTBHTLIST"
# echo "LHITSBHTLIST: $LHITSBHTLIST"
# echo "RHITSBHTLIST: $RHITSBHTLIST"
# 
# CACHEDIR="$(md5sum "$LHITSBHT" | awk '{print $1}')"
# mkdir -p "$CACHEDIR"
# echo "CACHEDIR: $CACHEDIR"
# 
# cat "$RHITSBHTLIST" | while read rhitsbht; do
# 	recipbht="$CACHEDIR/$(md5sum "$LHITSBHT" | awk '{print $1}')".bht
# 	echo "rhitsbht: $rhitsbht"
# 	echo "recipbht: $recipbht"
# 	cmd="reciprocal.R $recipbht $LHITSBHT $rhitsbht"
# 	echo "$cmd" && $cmd && echo "$recipbht" >> "$OUTBHTLIST"
# 	# each R script invocation needs: out, lhits, rhits
# done

# out, left, and right hit table lists
OBHTS="$1"; LBHTS="$2"; RBHTS="$3"

# sanity check: lists should be the same length
NLBHTs=$(wc -l $LBHTS | awk '{print $1}')
NRBHTS=$(wc -l $LBHTS | awk '{print $1}')
[[ $NLBHTS == $NRBHTS ]] || (echo "uh oh! lists are different lengths!"; exit 1)

# if empty lists, just touch output
[[ $NBHTS == 0 ]] && touch $OBHTS && exit 0

for n in {1..$NBHTS}; do
done
