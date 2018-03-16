#!/usr/bin/env python

# Convert a FASTA file to a list of genes.
# Usage: extract-seq-ids <tmpdir> <outgenes> <infasta>
# (tmpdir is passed as by ShortCut convention, but not used)

# TODO get this working with symlinks so it doesn't randomly fail on the wiki

from Bio import SeqIO
from sys import argv

# print "argv:", argv
outgenes = argv[1]
infasta  = argv[2]

uniq = set()

for seq in SeqIO.parse(infasta, 'fasta'):
    # line = '%s:%s\n' % (infasta, seq.id)
    line = '%s\n' % seq.id
    uniq.add(line)

# if len(uniq) == 0:
    # uniq = ["<<emptylist>>"]
# else:
uniq = list(uniq)
uniq.sort()

with open(outgenes, 'wb') as out:
    for line in uniq:
        out.write(line)
