#!/usr/bin/env python2

# Convert a list of genes back to FASTA.
# Usage: extract_seqs.py <outfasta> <infasta> <idlist>

from Bio import SeqIO
from sys import argv

outfa  = argv[1] # TODO pass tmpdir instead and have scripts assume 'result'
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
