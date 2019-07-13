#!/usr/bin/env python2

# simplest algorithm:
#
# take pident cutoff, list of rb_hits tables, list of more_hits tables
#
# for each zipped (rb_hits, more_hits) pair:
#   make a set of rb_hits qids
#   seed orthogoups from those qids
#   add more_hits sids above the pident cutoff to their orthogroup
#
# ...

# Simpler algorithm without loading the chlamy fasta:
# take cutoff fraction, list of rb_hits tables, list of more_hits tables
# for each zipped (rb_hits, more_hits) pair:
#   make a dict of qid:(cutoff, orthogroup) mappings
#     calculate pdist cutoff
#     assume the first one (qid?) is from the reference species
#     seed the orthogroup with the qid
#   for each hit in the more_hits table:
#     look up the qid in the orthogroups dict
#     ...

# from Bio import SearchIO
from sys import argv
from blast_parser import parse

def debug(*msg):
    print(msg)

# this is a generator
def read_hits(filename):
    with open(filename, 'r') as fh:
        # each BlastRecord is a list of hits with the same qid
        for blast_record in parse(fh):
            # within a record, need to iterate through two levels
            for hit in blast_record.hits:
                for hsp in hit:
                    yield hsp

# TODO is this only needed for the greencut1 algorithm, not 2?
def calc_pdists(gen):
    pdists = {}
    for hit in gen:
        if not hit.qid in pdists:
            pdists[hit.qid] = {}
        # TODO in rb_hits, there should only ever be one sid right?
        if not hit.sid in pdists[hit.qid]:
            pdists[hit.qid][hit.sid] = []
        pdist = 1 - (hit.pident / 100)
        assert pdist > 0, pdist < 1
        pdists[hit.qid][hit.sid].append(pdist)
    return pdists

def main(outpath, ref_faa, rb_hits, more_hits):
    debug("rb_hits:", rb_hits)
    d1 = calc_pdists(read_hits(rb_hits))
    print(d1)

if __name__ == '__main__':
    main(*argv[1:])
