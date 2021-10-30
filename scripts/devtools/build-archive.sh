#!/usr/bin/env bash
#
# Based on git-archive-all by Khaja Minhajuddin

set -e
set -C # noclobber

export ROOT_ARCHIVE_DIR="$(pwd)"
export BRANCH="$(git rev-parse --abbrev-ref HEAD)"
export TAR_PREFIX="ortholang-${BRANCH}"
export OUTPUT_FILE="${TAR_PREFIX}.tar.gz"

# create root archive
git archive --verbose --prefix "${TAR_PREFIX}/" --format "tar" --output "${ROOT_ARCHIVE_DIR}/archive-output.tar" "$BRANCH"

# for each of git submodules append to the root archive
git submodule foreach --recursive 'git archive --verbose --prefix=${TAR_PREFIX}/$path/ --format tar $BRANCH --output $ROOT_ARCHIVE_DIR/archive-output-sub-$sha1.tar'

if [[ $(ls archive-output-sub*.tar 2>/dev/null | wc -l) != 0  ]]; then
    # combine all archives into one tar
    tar --concatenate --file archive-output.tar archive-output-sub*.tar
    # remove sub tars
    rm -rf archive-output-sub*.tar
fi

# gzip the tar
gzip --force --verbose archive-output.tar

mv archive-output.tar.gz $OUTPUT_FILE
echo "final archive file:"
du -h "$OUTPUT_FILE"
