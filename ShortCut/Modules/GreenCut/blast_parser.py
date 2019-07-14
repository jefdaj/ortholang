#!/usr/bin/env python2

# This module is for handling BLAST output lists (in -outfmt 6/ -m8 format).
# It's documented on https://www.biostars.org/p/253984/
# TODO how to import it from greencut2_orthogroups.py?

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


def read(handle,eval_thresh=10, bitscore_thresh=0):
     """
     Read only one Blast record.
 
     USAGE:
     >>> import Blast
     >>> record = Blast.read(open('output.txt'))

     If the handle contains no records an exception is raised.
     If the handle contains multiple records, the first one is read.

     Use the Blast.parse(handle) function if you want
     to read multiple records from the handle.

     """
     iterator = parse(handle,eval_thresh,bitscore_thresh)
     try:
         first = next(iterator)
     except StopIteration:
         first = None
     return first
