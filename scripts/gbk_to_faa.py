#!/usr/bin/env python

# Convert a genbank file to FASTA (amino acid)
# Usage: gbk_to_faa.py <tmpdir> <outfaa> <ingbk>

# TODO do all genbank genomes have product qualifiers??

from Bio import SeqIO
from sys import argv

tmpdir = argv[1] # passed by convention but not used (TODO: remove?)
outfaa = argv[2]
ingbk  = argv[3]

# adapted from:
# http://www2.warwick.ac.uk/fac/sci/moac/people/students/peter_cock/python/genbank2fasta
with open(outfaa, 'w') as out:
  for seq_record in SeqIO.parse(ingbk, 'genbank'):
    for seq_feature in seq_record.features:
      if seq_feature.type == 'CDS':
        assert len(seq_feature.qualifiers['translation']) == 1
        assert len(seq_feature.qualifiers['product']) == 1
        # print seq_feature.qualifiers
        out.write(">%s %s\n%s\n" % (
          seq_feature.qualifiers['locus_tag'][0],
          seq_feature.qualifiers['product'][0],
          seq_feature.qualifiers['translation'][0]))
