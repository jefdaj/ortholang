#!/usr/bin/env python

# Convert a list of genes back to FASTA.
# Usage: extract-seqs-by-id <tmpdir> <outfasta> <ingenes> [<ingenes>...]

from Bio import SeqIO
from sys import argv

def read_genes(filenames):
    genes = {}
    for filename in filenames:
        with open(filename) as f:
            for line in f:
                line  = line.strip()
                path  = line[:line.find(':')]
                seqid = line[line.find(':')+1:]
                if not path in genes:
                    genes[path] = set()
                genes[path].add(seqid)
    return genes

def read_and_process_seqs(genes):
    seqs = []
    for path in genes:
        for seq in SeqIO.parse(path, 'fasta'):
            if not seq.id in genes[path]:
                continue
            # add path to id to help filter blast hits later
            seq.id = ':'.join((path, seq.id))

            # For some reason I don't understand, BioPython copies the ID
            # to the description here, so the output file ends up having
            # every ID written twice. Then none of them match up in R!
            # This kludge fixes it.
            seq.description = ''

            seqs.append(seq)
    return seqs

def write_seqs(seqs, filename):
    SeqIO.write(seqs, filename, "fasta")

def main():
    # tmpdir is passed as argv[1] by ShortCut convention, but not used
    genes = read_genes(argv[3:])
    seqs = read_and_process_seqs(genes)
    write_seqs(seqs, argv[2])

if __name__ == '__main__':
    main()
