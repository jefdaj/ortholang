#!/usr/bin/env python

# Convert a list of genes back to FASTA.
# Usage: extract-seqs-by-id <tmpdir> <outfasta> <infasta> <idlist>
# TODO why does this create an empty fasta file??

from Bio import SeqIO
from sys import argv

tmpdir = argv[1] # passed by convention, but not used here TODO remove?
outfa  = argv[2]
infa   = argv[3]
inids  = argv[4]

with open(inids, 'r') as iis:
    ids = set(i.strip() for i in iis.readlines())

with open(outfa, 'wb') as out:
    for s in SeqIO.parse(infa , 'fasta'):
        if s.id in ids:
            SeqIO.write(s, out, 'fasta')
