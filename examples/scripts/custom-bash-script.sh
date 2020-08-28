#!/usr/bin/env bash

# There will always be 3 arguments from OrthoLang
#
# 1. the final path where this script should write its result
# 2. a list of variable names, which may be empty or incomplete depending on the orhtholang script
# 3. a list of the corresponding variable values (mostly paths, but may include literals)

output_path="$1"
names_path="$2" # we'll ignore these because they're tricky to use in bash
values_path="$3"

# This script is meant to be run by tests/scripts/scripts:bash_tutorial.ol,
# which passes it a str, num, num.list, and str.list in that order.
path1="$(sed -n1 "$values_path")"
path2="$(sed -n2 "$values_path")"
path3="$(sed -n3 "$values_path")"

# We'll follow best practices by only "printing" to the output file.
# You can print stuff while debugging of course, but for production it's better to be quiet
# and redirect messages to a log file if needed.
log_path="${output_path}.log"

# To read a str, num, str.list, or num.list, just use cat. Nothing special required.
echo "the num is: $(cat "$path1")" &> "$log_path"
# echo "the str.list is: $(cat "$path1")" &> "$log_path"

# to read a list of anything else, you'll need to substitute TMPDIR and WORKDIR
# into all the paths. here's one way to do it with substenv from GNU gettext:
# cat numlistlist.txt | substenv | while read path; do
  # echo "reading nums from ${path}..."
	# if the paths are nested further, use substenv again here:
	# cat $path
# done

# you could work out named arrays if you really want to,
# but at that point you might as well switch another language like R or Python
