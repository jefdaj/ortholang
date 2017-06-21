#!/usr/bin/env python

# Convert a FASTA file to a list of genes.
# Usage: extract-seq-ids <tmpdir> <outgenes> <infasta>
# (tmpdir is passed as by ShortCut convention, but not used)

# TODO get this working with symlinks so it doesn't randomly fail on the wiki

from Bio import SeqIO
from sys import argv

outgenes = argv[2]
infasta  = argv[3]

with open(outgenes, 'wb') as out:
    for seq in SeqIO.parse(infasta, 'fasta'):
        # line = '%s:%s\n' % (infasta, seq.id)
        line = '%s\n' % seq.id
        out.write(line)
