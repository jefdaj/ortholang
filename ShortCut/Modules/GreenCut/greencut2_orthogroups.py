#!/usr/bin/env python2

# This is the simplest way I could come up with to implement the GreenCut2 algorithm:
#
# Predicted protein sequences from sequenced genomes were subjected to
# phylogenomics analysis using methods described previously (22). Briefly,
# WU-BLASTP searches were conducted between the C. reinhardtii (JGI v3.1)
# predicted proteome and the predicted proteomes from a phylogenetically
# diverse set of organisms (listed below). A mutual best BLASTP hit (E-value <1e-10)
# was used to establish orthology to a Chlamydomonas protein. Additional
# eukaryotic proteins that were not a mutual best hit but had >50% amino acid
# identity to a Chlamydomonas protein within an ortholog cluster were selected as
# in-paralogs (co-orthologs throughout).

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

def start_families(rb_gen):
    families = {}
    for hsp in rb_gen:
        if not hsp.qid in families:
            families[hsp.qid] = set()
        # debug('before: %s' % families[hsp.qid])
        # debug('hsp: %s' % hsp.__dict__)
        families[hsp.qid].add(hsp.qid)
        families[hsp.qid].add(hsp.sid)
        # debug('after: %s' % families[hsp.qid])
    return families

def extend_families(families, more_gen):
    for hsp in more_gen:
        # debug('qid: %s sid: %s' % (hsp.qid, hsp.sid))
        try:
            families[hsp.qid].add(hsp.sid)
        except KeyError:
            # debug(hsp.qid, hsp.sid)
            # debug(hsp.qid in families.keys())
            # debug(families.keys()[:10])
            continue
    return families # TODO unnecessary?

def merge_families(sets, ref_set):
    '''
    based on https://stackoverflow.com/a/9112588
    but this version only considers ref_set elements when merging
    '''
    merged = True
    while merged:
        merged = False
        results = []
        while sets:
            common, rest = sets[0], sets[1:]
            sets = []
            for x in rest:
                # TODO is this as efficient as it can be?
                if x.intersection(ref_set).isdisjoint(common.intersection(ref_set)):
                    sets.append(x)
                else:
                    merged = True
                    common |= x
            results.append(common)
        sets = results
    return sets

def save_families(fams, ref_set, filename):
    with open(filename, 'w') as f:
        # TODO sort fams largest first?
        for fam in fams:
            if len(fam) < 2:
                continue
            refs = fam.intersection(ref_set)
            rest = fam.difference(ref_set)
            f.write('%s\t:\t%s\n' % ('\t'.join(refs), '\t'.join(rest)))

def main(out_bht, rb_hits, more_hits):
    # debug("rb_hits:", rb_hits)
    # debug("more_hits:", more_hits)
    fams = start_families(read_hits(rb_hits))
    refs = set(fams.keys())
    fams = extend_families(fams, read_hits(more_hits))
    fams = merge_families(fams.values(), refs)
    save_families(fams, refs, out_bht)
    return fams

if __name__ == '__main__':
    main(*argv[1:])
