#!/usr/bin/env bash

# Semi-automated rebase for integrating features into the module branches.
# Do a couple modules manually, then edit this to repeat for the rest.

# Example usage for the HMMER branch after editing:
# ./scripts/rebase-module.sh module-hmmer hmmer

set -e
set -x

this_branch=$1
git checkout $this_branch
git rebase master || git status

# Steps here vary depending on recent changes. Example:
# this_test=$2
# git checkout master test.sh
# sed -i "s/test_branch='all'/test_branch='${this_test}'/g" test.sh
# git add test.sh
# git rebase --continue || git rebase --skip

./test.sh

# If that goes well, git push --force manually
