#!/usr/bin/env python

from os         import remove
from os.path    import exists, join
from subprocess import check_call
from sys        import argv

def read_lines(filename):
    with open(filename, 'r') as f:
        return [l.strip() for l in f.readlines()]

def write_lines(lines, filename):
    # try:
    with open(filename, 'w') as f:
        for line in lines:
            f.write('%s\n' % line)
    # except:
    #     if exists(filename):
    #         remove(filename)
    # finally:
    #     raise SystemExit(1)

def tmppath(tmpdir, left, right):
    # TODO need full path here
    name = str(abs(hash(left))) + '_' + str(abs(hash(right))) + '.bht'
    return join(tmpdir, name)

def reciprocal(out, left, right):
    # TODO make sure nix can find this!
    cmd = ["reciprocal.R", out, left, right]
    return check_call(cmd)

def main():
    tmpdir  = argv[1]
    outpath = argv[2]
    lefts   = read_lines(argv[3])
    rights  = read_lines(argv[4])
    if not len(lefts) == len(rights):
        print 'lists are different lengths! %d vs %d' % (len(lefts), len(rights))
        raise SystemExit(1)
    pairs = zip(lefts, rights)
    outs = []
    for (left, right) in pairs:
        out = tmppath(tmpdir, left, right)
        reciprocal(out, left, right)
        outs.append(out)
    write_lines(outs, outpath)

if __name__ == '__main__':
    main()
