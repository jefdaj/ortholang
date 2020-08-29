#!/usr/bin/env python3

# The top ~half of this code (first 3 functions) was modified from
# https://pypi.org/project/deterministic-zip v0.1 and uses the
# simplified BSD license.

# TODO use this to write an example custom python script too
# TODO redirect stderr to log file

import os
import stat
import sys
import zipfile

def add_directory(zip_file, path, zip_path):
    for item in sorted(os.listdir(path)):
        current_path = os.path.join(path, item)
        current_zip_path = os.path.join(zip_path, item)
        if os.path.isfile(current_path):
            add_file(zip_file, current_path, current_zip_path)
        else:
            add_directory(zip_file, current_path, current_zip_path)

def add_file(zip_file, path, zip_path=None):
    permission = 0o555 if os.access(path, os.X_OK) else 0o444
    zip_info = zipfile.ZipInfo.from_file(path, zip_path)
    zip_info.date_time = (2019, 1, 1, 0, 0, 0)
    zip_info.external_attr = (stat.S_IFREG | permission) << 16
    with open(path, 'rb') as fp:
        zip_file.writestr(zip_info, fp.read(),
                          compress_type=zipfile.ZIP_DEFLATED, compresslevel=9)

def deterministic_zip(output_path, input_paths):
    failure = False
    with zipfile.ZipFile(output_path, 'w') as zip_file:
        for path in sorted(input_paths):
            if os.path.isdir(path):
                add_directory(zip_file, path, os.path.basename(path))
            elif os.path.isfile(path):
                add_file(zip_file, path, os.path.basename(path))
            else:
                sys.stderr.write('Invalid PATH: {}\n'.format(path))
                failure = True
                break
    if failure:
        os.unlink(output_path)
        return 1
    sys.stderr.write('Wrote {}'.format(output_path))
    return 0

def main(output_path, names_path, paths_path):

    output_dir = os.dirname(output_path)
    input_dir = os.path.join(output_dir, 'ortholang_result') # TODO better name with hash(es)?
    print('input_dir: {}'.format(input_dir))
    os.makedirs(input_dir)

    # note that this is an unrelated meaning for zip
    with open(names_path, 'r') as f:
        names = f.readlines()
    with open(paths_path, 'r') as f:
        paths = f.readlines()
    pairs = zip(names, paths)
    print('pairs: {}'.format(pairs))

    # deterministic_zip(output_path, [input_dir])

if __name__ == '__main__':
    main(sys.argv[1], sys.argv[2], sys.argv[3])
