#!/usr/bin/env python3

# The top ~half of this code (first 3 functions) was modified from
# https://pypi.org/project/deterministic-zip v0.1 and uses the
# simplified BSD license.

# TODO use this to write an example custom python script too

import os
import stat
import sys
import zipfile
import shutil

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

def write_ortholang_list(input_dir, path, name):
    input_dir = os.path.join(input_dir, name.split('.')[0])
    ext = name[name.find('.'):]
    os.makedirs(input_dir, exist_ok=True)
    os.chdir(dst)
    with open(path, 'r') as f:
        paths = [l.rstrip() for l in f.readlines()]
    # picking up nested list names sounds too error-prone, so we just do "element1", ...
    # TODO pad with zeros?
    index = 0
    for path in paths:
        index += 1
        name = "element" + str(index) + ext
        write_ortholang_arg(input_dir, path, name) # is the recursion ok?

def write_ortholang_arg(input_dir, path, name):
    dst = os.path.join(input_dir, name)
    exts = name.split('.')[1:]
    print('locals:', locals())

    if exts == ['str'] or exts == ['num']:
        # case 1: "path" is actually a single lit (num or str) which should be written to a file
        with open(dst, 'w') as f:
            f.write(path + '\n')

    elif exts == ['str', 'list'] or exts == ['num', 'list'] or exts[-1] is not 'list':
        # case 2: path is to a literal list (num.list, str.list) which should be copied over as is
        # case 3: path is to a single non-lit type and should be copied over as is
        shutil.copyfile(path, dst)

    else:
        # case 4: path is to a list of non-lit type, so we should make a dir + copy elements into it
        write_ortholang_list(input_dir, path, name) # is the recursion ok?

def main(output_path, names_path, paths_path):
    output_dir = os.path.dirname(output_path)
    os.chdir(output_dir)

    # put files in a uniquely-named dir to avoid confusion when unzipping
    uniq = str(abs(hash(os.path.relpath(output_dir, os.path.expandvars('$TMPDIR')))))[:10]
    input_dir = os.path.join(output_dir, 'ortholang_{}'.format(uniq))
    os.makedirs(input_dir, exist_ok=True)

    # pair names with their paths
    # note that this is an unrelated meaning of zip
    with open(names_path, 'r') as f:
        names = f.readlines()
    with open(paths_path, 'r') as f:
        paths = f.readlines()

    # TODO this shouldn't be needed!
    # fix for the case where the only arg is a list, and the names have been picked from inside it
    while 'exprs/list/' in paths[0] and not names[0].endswith('.list'):
        paths_path = os.path.expandvars(paths[0].rstrip())
        # print('locals:', locals())
        with open(paths_path, 'r') as f:
            paths = f.readlines()
    # print('paths_path: {}'.format(paths_path))

    for (path, name) in zip(paths, names):
        path = os.path.expandvars(path.rstrip())
        name = name.rstrip()

        if name == '<<emptylist>>':
            # argument list is empty; write a single text file signifying that
            path = os.path.join(input_dir, 'result.list')
            with open(path, 'w') as f:
                f.write(name + '\n')
            break

        else:
            # write the arguments to a folder as planned
            write_ortholang_arg(input_dir, path, name)

    # zip up and delete the input_dir
    deterministic_zip(output_path, [input_dir])
    shutil.rmtree(input_dir)

if __name__ == '__main__':
    main(sys.argv[1], sys.argv[2], sys.argv[3])
