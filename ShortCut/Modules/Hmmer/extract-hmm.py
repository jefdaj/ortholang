#!/usr/bin/env python2

# Extract a column from the Hmmer results, getting around a couple quirks
# TODO does it need to handle an <<empty*>>?

from sys import argv

OUTPATH = argv[1]
TSVPATH = argv[2]
COLNUM  = int(argv[3])

with open(TSVPATH, 'r') as f:
    words = set(line[:-1].split()[COLNUM-1] \
                for line in f.readlines() \
                if not line.startswith('#'))

with open(OUTPATH, 'w') as f:
    for w in words:
        f.write(w + '\n')
