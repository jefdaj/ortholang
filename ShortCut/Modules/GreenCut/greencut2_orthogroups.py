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

# from blast_parser import parse
# pasting blast_parser code instead of importing for now...
# TODO figure out the proper way to import

from itertools import groupby

class Hsp:
    """Store information about single HSP in an alignment hit. 

    Members: 
    qid       Query Id
    sid       Subject Id
    pident    Percentage of identical matches
    length    Alignment length
    mismatch  Number of mismatches
    gaps      Total number of gaps
    qstart    Start of alignment in query
    qend      End of alignment in query
    sstart    Start of alignment in subject
    send      End of alignment in subject
    evalue    Expect value
    bitscore  Bit score

    USAGE:
    >>> line = 'qid\tsid\t83.99\t37\t14\t15\t1\t147\t1\t149\t0.0\t219\n'
    >>> hsp = Hsp(line)
    >>> hsp.bitscore
    219
    >>> hsp.pident
    83.99

    """

    def __init__(self,entry):
        bt_fields = entry.split('\t')
        self.qid = bt_fields[0]
        self.sid = bt_fields[1]
        self.pident = float(bt_fields[2])
        self.length = int(bt_fields[3])
        self.mismatch = int(bt_fields[4])
        self.gaps = int(bt_fields[5])
        self.qstart = int(bt_fields[6])
        self.qend = int(bt_fields[7])
        self.sstart = int(bt_fields[8])
        self.send = int(bt_fields[9])
        self.evalue = float(bt_fields[10])
        self.bitscore = float(bt_fields[11])

    def _format(self):
        l = [self.qid, 
             self.sid, 
             '{0:.2f}'.format(self.pident),
             '{0}'.format(self.length),
             '{0}'.format(self.mismatch),
             '{0}'.format(self.gaps),
             '{0}'.format(self.qstart),
             '{0}'.format(self.qend),
             '{0}'.format(self.sstart),
             '{0}'.format(self.send),
             '{:.1E}'.format(self.evalue),
             '{0}'.format(self.bitscore)
            ]
        return l

    def format(self):
        return '\t'.join(self._format())

    def __str__(self):
        f = "{0}\t{1}".format(self.qid, self.sid)
        return f





class BlastRecord:
    """Object representing a Blast Record. 

    Arguments: 
    qid       - Query sequence id
    hits      - Blast hits

    """

    def __init__(self, qid=None, hits=None):
        """Initialize Blast Record instance"""
        self.qid = qid
        self.hits = hits

    def evalue_cutoff(self, evalue):
        """Filter HSPs by given e-value."""
        l = []
        for hit in self.hits:
            hsps = []
            for hsp in hit:
                if hsp.evalue <= evalue:
                    hsps.append(hsp)
            if hsps: l.append(hsps)
        self.hits = l

    def best_hits(self):
        """Return list of first hits that obtain the 
           same score.
        """
        l = []
        if not self.hits: return l
        else:
            max_score = self.hits[0][0].bitscore
            for hit in self.hits:
                if hit[0].bitscore >= max_score:
                    l.append(hit)
                else:
                    break
            return l

    def best_hsps(self):
        """Return list of first HSPs that obtain the 
           same score.
        """
        l = []
        if not self.hits: return l
        else:
            max_score = self.hits[0][0].bitscore
            for hit in self.hits:
                if hit[0].bitscore >= max_score:
                    l.append(hit[0])
                else:
                    break
            return l

    def best_hsps_except_query(self):
        """Return list of first HSPs that obtain the 
           same score.
        """
        l = []
        if not self.hits: return l
        else:
            max_score = None
            for hit in self.hits:
                if hit[0].sid != self.qid:
                    if not max_score or hit[0].bitscore>=max_score:
                        max_score = hit[0].bitscore
                        l.append(hit[0])
                    else:
                        break
            return l

    def _format(self):
        l = []
        for hit in self.hits:
            for hsp in hit:
                l.append(hsp._format())
        return l        

    def format(self):
        """Return output string of BLAST record"""
        l = []
        for hit in self.hits:
            for hsp in hit:
                l.append(hsp.format())
            l.append('')
        return "\n".join(l).strip()

    def __str__(self):
        """Return output string of BLAST record"""
        return self.format()


#This is a generator function!
def parse(handle, eval_thresh=10, bitscore_thresh=0):
     """Generator function to iterate over Blast records.
     
     Arguments: 
     handle      - input file handle containg Blast tabular 
                   outputs (-outfmt 6, -m8).
     eval_thresh - E-value cutoff for Blast results.

     """
     for qid, blasts in groupby(handle, lambda l: l.split()[0]):
         hits = []
         prev_sid = False
         for sid, hsps in groupby(blasts, lambda l: l.split()[1]):
             hsps_temp = []
             for line in hsps:
                 line = line.decode("utf-8")
                 hsp = Hsp(line)
                 if hsp.bitscore >= bitscore_thresh and hsp.evalue <= eval_thresh:
                     hsps_temp.append(hsp)
             if hsps_temp: hits.append(hsps_temp)
         yield BlastRecord(qid=qid, hits=hits)


# the rest is my code

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
