#!/usr/bin/env python

# Tranlate a FASTA amino acid file to nucleic acids.
# see http://biopython.org/wiki/Seq

from Bio import SeqIO
from sys import argv

outfna = argv[1]
infaa  = argv[2]

with open(outfna, 'wb') as out:
    for rec in SeqIO.parse(infaa, 'fasta'):
        rec.seq = rec.back_transcribe()
        SeqIO.write(rec, out, 'fasta')
