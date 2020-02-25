#!/usr/bin/env python2

# TODO move to Core somehow?
# Sanitize FASTA sequence IDs by putting replacing them with their hashes.
# Usage: hash_seqids.py <outfasta> <infasta>
# The sanitized fasta will go in <outfasta>, with IDs in <outfasta>.ids
# In the context of OrthoLang, <outfasta> is expected to be like: $TMPDIR/cache/load/<md5sum>

from Bio import SeqIO
from sys import argv
from hashlib import md5

def hash_id(seqid):
    return 'seqid_' + str(md5(seqid).hexdigest())[:11]

outfa = argv[1]
outids = outfa + '.ids'
infa = argv[2]

# all hash -> seqid pairs so far
SEQIDS = {}

with open(outfa, 'wb') as out:
    for seq in SeqIO.parse(infa, 'fasta'):
        hashed = hash_id(seq.description)
        if hashed in SEQIDS:
            raise Exception('duplicate seqid: %s -> %s' % (hashed, seq.description))
        SEQIDS[hashed] = seq.description
        seq.id = hashed
        seq.description = hashed
        SeqIO.write(seq, out, 'fasta')

# TODO does SEQIDS need any sorting?
with open(outids, 'wb') as out:
    # TODO also write the fasta name itself at the top (how to do $TMPDIR?)
    out.write('%s\t%s\n' % ('$TMPDIR thing here', 'actual load path here'))
    for hashed in SEQIDS:
        out.write('%s\t%s\n' % (hashed, SEQIDS[hashed]))
