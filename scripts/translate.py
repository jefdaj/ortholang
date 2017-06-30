#!/usr/bin/env python

# Tranlate a FASTA nulceic acid file to amino acids.
# Assumes the first reading frame.
# see http://biopython.org/wiki/Seq

from Bio import SeqIO
from sys import argv

outfaa = argv[1]
infna  = argv[2]

with open(outfaa, 'wb') as out:
    for rec in SeqIO.parse(infna, 'fasta'):
        rec.seq = rec.seq.translate()
        SeqIO.write(rec, out, 'fasta')
