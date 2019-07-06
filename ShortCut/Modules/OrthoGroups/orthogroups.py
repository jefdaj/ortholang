#!/usr/bin/env python3

# TODO would it help to use a set/frozenset for the outer list too?
# TODO how to handle the numerical arguments from Haskell?

from sys import argv

def debug(msg):
    # print(msg)
    pass

def read_idlists(listpath):
    sets = []
    with open(listpath, 'r') as l:
        for setpath in l.read().split('\n'):
            if len(setpath) == 0:
                continue
            if debug:
                debug('reading setpath {}'.format(setpath))
            with open(setpath, 'r') as s:
                sets.append(frozenset(i for i in s.read().split('\n') if len(i) > 0))
    return sets

def read_families(listpath):
    # same as idlists, except it returns tuples with the family path to write at the end
    sets = []
    with open(listpath, 'r') as l:
        for setpath in l.read().split('\n'):
            if len(setpath) == 0:
                continue
            if debug:
                debug('reading setpath {}'.format(setpath))
            with open(setpath, 'r') as s:
                sets.append((setpath, frozenset(i for i in s.read().split('\n') if len(i) > 0)))
    return sets

def test_min(ids, min_matches, fam):
    matches = 0
    for idset in ids:
        if len(fam & idset) > 0:
            matches += 1
        if matches >= min_matches:
            return True
    return False

def test_max(ids, max_matches, fam):
    matches = 0
    for idset in ids:
        if len(fam & idset) > 0:
            matches += 1
        if matches > max_matches:
            return False
    return True

TESTS = {'min': test_min, 'max': test_max}

def main(outpath, fnname, fnarg, famspath, idspath):
    # note fnarg is ignored if fnname is any or all
    debug('famspath: {}'.format(famspath))
    debug('idspath: {}'.format(idspath))
    fams  = read_families(famspath)
    ids   = read_idlists(idspath)
    if fnname == 'all':
        fnname = 'min'
        fnarg = len(fams)
    elif fnname == 'any':
        fnname = 'min'
        fnarg = 1
    else:
        fnarg = int(fnarg)
        debug('fnarg: {}'.format(fnarg))
    debug('fnname: {}'.format(fnname))
    debug('outpath: {}'.format(outpath))
    # TODO read numeric argument here, but not always? maybe duplicate the script?
    fams2 = [f for f in fams if TESTS[fnname](ids, fnarg, f[1])]
    debug(fams2) # TODO save properly
    with open(outpath, 'w') as f:
        for (path, _) in fams2:
            f.write('{}\n'.format(path))

if __name__ == '__main__':
    main(*argv[1:])
