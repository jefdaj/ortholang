#!/usr/bin/env python

# Convert a list of genes back to FASTA.
# Usage: extract-seqs-by-id <tmpdir> <outfasta> <infasta> <idlist>
# TODO why does this create an empty fasta file??

from Bio import SeqIO
from sys import argv

# tmpdir = argv[1] # passed by convention, but not used here TODO remove?
outfa  = argv[1]
infa   = argv[2]
inids  = argv[3]

with open(inids, 'r') as iis:
    ids = set(i.strip() for i in iis.readlines())

with open(outfa, 'wb') as out:
    for s in SeqIO.parse(infa , 'fasta'):
        if s.id in ids:
            SeqIO.write(s, out, 'fasta')
