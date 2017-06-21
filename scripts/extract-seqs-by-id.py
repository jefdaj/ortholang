#!/usr/bin/env python

# Convert a list of genes back to FASTA.
# Usage: extract-seqs-by-id <tmpdir> <outfasta> <infasta> <idlist>

from Bio import SeqIO
from sys import argv

def read_ids(filename):
    with open(filename, 'r') as f:
        return [l.strip() for l in f.readlines()]

def filter_seqs(fasta, ids):
    print ids
    print fasta
    return list(s for s in SeqIO.parse(fasta, 'fasta') if s.id in ids)

def write_seqs(seqs, filename):
    SeqIO.write(seqs, filename, "fasta")

def main():
    # tmpdir is passed as argv[1] by ShortCut convention, but not used here
    fasta = argv[3]
    ids   = read_ids(argv[4])
    seqs  = filter_seqs(fasta, ids)
    write_seqs(seqs, argv[2])

if __name__ == '__main__':
    main()
