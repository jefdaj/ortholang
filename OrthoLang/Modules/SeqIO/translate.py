#!/usr/bin/env python3

# TODO fix this for newer biopython, and remove my version?

# Tranlate a FASTA nulceic acid file to amino acids.
# Assumes the first reading frame.
# see http://biopython.org/wiki/Seq

from Bio import SeqIO
from sys import argv

# TODO don't suppress these?? maybe just re-throw without filenames?
import warnings
from Bio import BiopythonWarning
warnings.simplefilter('ignore', BiopythonWarning)

outfaa = argv[1]
infna  = argv[2]

with open(outfaa, 'w') as out:
    for rec in SeqIO.parse(infna, 'fasta'):
        rec.seq = rec.seq.translate()
        SeqIO.write(rec, out, 'fasta')
