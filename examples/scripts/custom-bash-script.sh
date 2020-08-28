#!/usr/bin/env bash

# to read a str, num, str.list, or num.list, just use cat. nothing special required.
# for example:
cat numlist.txt

# to read a list of anything else, you'll need to substitute TMPDIR and WORKDIR
# into all the paths. here's one way to do it with substenv from GNU gettext:
cat numlistlist.txt | substenv | while read path; do
  echo "reading nums from ${path}..."
	# if the paths are nested further, use substenv again here:
	cat $path
done

# you could work out named arrays if you really want to,
# but at that point you might as well switch another language like R or Python
