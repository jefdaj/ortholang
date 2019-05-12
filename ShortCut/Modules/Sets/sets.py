#!/usr/bin/env python2

# Set operations. They can easily be done in Haskell, but this pulls them out
# of the main processing thread so they can be done on another node if needed.

# TODO handle <<empty*>> sets

from sys import argv

def is_empty_set(s):
    if len(s) != 1:
        return False
    for e in s:
        return e.startswith('<<empty')

def read_set(filename):
    with open(filename, 'r') as f:
        s = set(f.readlines())
    if is_empty_set(s):
        return set()
    else:
        return s

def read_sets(listpath):
    with open(listpath, 'r') as f:
        return [read_set(p) for p in f.read().split()]

def set_any(sets):
    out = set()
    for s in sets:
        out.update(s)
    return out

def set_all(sets):
    if len(sets) == 0:
        return set()
    out = sets[0]
    for s in sets[1:]:
        out.difference_update(s)
    return out

def main(outpath, setop, paths):
    setops = {
        'difference': lambda ss: ss[0] - ss[1],
        'intersection': lambda ss: ss[0] & ss[1],
        'union': lambda ss: ss[0] | ss[1],
        'any': lambda ss: set_any(ss),
        'all': lambda ss: set_all(ss)
        # TODO 'some'
    }
    if len(paths) == 1:
        # path is to a list of set paths
        sets = read_sets(paths[0])
    else:
        # paths are to the sets themselves
        sets = [read_set(p) for p in paths]
    out = setops[setop](sets)
    with open(outpath, 'w') as f:
        f.writelines(out)

if __name__ == '__main__':
    main(argv[1], argv[2], argv[3:])
