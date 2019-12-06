#!/usr/bin/env python2

# Convert a list of genes back to FASTA.
# Usage: extract-seqs-by-id <tmpdir> <outfasta> <infasta> <idlist>
# TODO why does this create an empty fasta file??

from Bio import SeqIO
from sys import argv

# tmpdir = argv[1] # passed by convention, but not used here TODO remove?
outfa  = argv[1]
infa   = argv[2]
inids  = argv[3]

def matches_prefix(seqid, seqids_to_keep):
    # seqid = seqid.split()[0]
    for i in seqids_to_keep:
        if seqid.startswith(i):
            # print "'%s' matches '%s'" % (seqid, i)
            return True
    # print "warning: no match for seqid '%s'" % seqid
    return False

with open(inids, 'r') as iis:
    ids = set(i.strip() for i in iis.readlines())

with open(outfa, 'wb') as out:
    for s in SeqIO.parse(infa , 'fasta'):
        if matches_prefix(s.id, ids):
            SeqIO.write(s, out, 'fasta')
