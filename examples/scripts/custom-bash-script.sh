#!/usr/bin/env bash

# This script is meant to be run by tests/scripts/scripts:bash_tutorial.ol,
# which passes it a str, num, num.list, str.list, and num.list.list in that
# order. It demonstrates how to work with each one in Bash, and writes everything
# to the main output file. As you can see, the quoting and echoing and
# envsubsting get complicated! I suggest switching to R, Python, or a similar
# scripting language if you need to read more than a couple nested lists.

# There will always be 3 arguments from OrthoLang:
#
# 1. the final path where this script should write its result
# 2. a list of variable names, which may be empty or incomplete depending on the orhtholang script
# 3. a list of the corresponding variable paths
output_path="$1"
names_path="$2"
paths_path="$3"

# You can also create a log file you want, but we won't use it in this example:
log_path="${output_path}.log"

# This reads a single name from the names file
varname() { sed -n ${1}p "$names_path"; }

# This reads a single line from the values file and "absolutizes" the path
# by substituting in OrthoLang's TMPDIR and WORKDIR environment variables.
varpath() { sed -n ${1}p "$paths_path" | envsubst; }

# We'll follow best practices by only "echoing" to the output file. You can
# print stuff while debugging of course, but for production it's better to be
# quiet. If needed, you can also redirect to the log file.
output() { echo "$@" &>> "$output_path"; }

# The simplest way to read a string is to cat out the corresponding path.
output "input 1 is a str named '`varname 1`' with the value '`cat $(varpath 1)`'"
output

# Same with a num,
output "input 2 is a num named '`varname 2`' with the value '`cat $(varpath 2)`'"
output

# or a num.list,
output "input 3 is a num.list named '`varname 3`' and contains these nums:"
output "`cat $(varpath 3)`"
output

# or a str.list. Here we also demonstrate quoting the individual strings though.
output "input 4 is a str.list named '`varname 4`' and contains these strs:"
cat $(varpath 4) | while read line; do
  output "'$line'"
done
output

# To read a nested list of lists, you just have to remember to use envsubst each time.
# Here we show the raw path from OrthoLang, then use envsubst before reading it.
output "input 5 is a num.list.list named '`varname 5`' and contains these num.lists:"
cat $(varpath 5) | while read list_path; do
  output "  '$list_path', which contains these nums:"
	cat `echo "$list_path" | envsubst` | while read n; do
	  output "    $n"
	done
done
