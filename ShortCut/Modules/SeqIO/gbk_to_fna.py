#!/usr/bin/env python2

# Convert a genbank file to FASTA (amino acid)
# Usage: gbk_to_fna.py <tmpdir> <outfna> <ingbk>

# TODO make a note that this only extacts CDS features? call it gbk_to_fna_genes?
# TODO do all genbank genomes have product qualifiers??
# TODO add qualifiers['gene'] to description if present

from Bio import SeqIO
from sys import argv

# silence warnings
import warnings
from Bio import BiopythonParserWarning
warnings.simplefilter('ignore', BiopythonParserWarning)

# tmpdir = argv[1] # passed by convention but not used (TODO: remove?)
outfna = argv[1]
ingbk  = argv[2]

def seqid(seq_feature):
    seqid = []
    # pick an id
    for q in ['locus_tag', 'protein_id']:
        if q in seq_feature.qualifiers:
            seqid = seq_feature.qualifiers[q]
            break
    if not seqid:
        raise Exception('no seqid found for feature: %s' % seq_feature)
    # add other info as the description
    desc = []
    desckeys = ['gene', 'product', 'inference', 'note']
    if 'locus_tag' in seq_feature.qualifiers:
        desckeys = ['protein_id'] + desckeys
    for q in desckeys:
        try:
            val = seq_feature.qualifiers[q][0]
            if not 'ORF_ID' in val:
                desc.append(val)
        except:
            pass
    seqid = ' '.join(seqid + desc)
    return seqid

# adapted from:
# http://www2.warwick.ac.uk/fac/sci/moac/people/students/peter_cock/python/genbank2fasta
with open(outfna, 'w') as out:
  for seq_record in SeqIO.parse(ingbk, 'genbank'):
    for seq_feature in seq_record.features:
      if seq_feature.type != 'CDS': # TODO anything else we might want?
        continue
      # assert len(seq_feature.qualifiers['locus_tag']) == 1
      # assert len(seq_feature.qualifiers['product']) == 1
      # assert len(seq_feature.qualifiers['translation']) == 1
      sid = seqid(seq_feature)
      seq   = seq_feature.extract(seq_record.seq)
      out.write(">%s\n%s\n" % (sid, seq))
