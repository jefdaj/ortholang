#!/usr/bin/env bash

# Github releases fail to include submodules, so we have to fix them manually.
# Push the release tag, run this, then go to the "edit tag" page on Github and attach the files.
# Usage: ./dev-scripts/build-release-archives.sh <git tag>

set -E

tag="$1"

rm -rf release-${tag}
mkdir release-${tag}
pushd release-${tag}

git clone http://github.com/jefdaj/shortcut
pushd shortcut
git checkout ${tag}
git submodule update --init --recursive
rm -rf .git*
popd

tar cvzf shortcut-${tag}.tar.gz shortcut
zip -r   shortcut-${tag}.zip    shortcut

echo "release files:"
du -h shortcut-*
