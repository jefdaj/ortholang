"""This module is used to execute statistics on raw reads of preprocessed files, as well as SAM abd BAM files."""
from collections import OrderedDict #allows the creations of ordered dictionaries
import numpy as np
import math
import os
import sys
import sys_tools as systools


#variables
__module_name__ = 'Reads Stats'
__source__ = 'reads_stats.py'
__author__ = 'Salvatore Cosentino'
#__copyright__ = ''
__license__ = 'GPL'
__version__ = '0.2'
__maintainer__ = 'Cosentino Salvatore'
__email__ = 'salvo981@gmail.com'

#get the path to the directory where the scripts are stored
pySrcDir = os.path.dirname(os.path.abspath(__file__))


def info():
    """Thi module is used to execute statistics on raw reads of preprocessed files, as well as SAM abd BAM files."""
    print('MODULE NAME:\t%s'%__module_name__)
    print('SOURCE FILE NAME:\t%s'%__source__)
    print('MODULE VERSION:\t%s'%__version__)
    print('LICENSE:\t%s'%__license__)
    print('AUTHOR:\t%s'%__author__)
    print('EMAIL:\t%s'%__email__)



def calcSeqStats(inSeq, debug=False):
    """Calculate simple stats about the input sequence (max, min, avg, std length etc.)."""
    from Bio import SeqIO
    if debug:
        print('calcSeqStats::START')
        print('INPUT SEQ ::\t%s'%inSeq)
    #check the existence of the input file
    if not os.path.isfile(inSeq):
        sys.stderr.write('The file %s was not found, please produce the contigs file before proceding'%inSeq)
        sys.exit(-2)
    #check that the file is not empty
    stInfo = os.stat(inSeq)
    if stInfo.st_size == 0:
        sys.stderr.write('\nWARNING: the input file is empty, a tuple with 0 values will be returned.\n')
        return (0, 0, 0., 0., 0)
    tmpList = []#will contain the length values before they are inserted in a numpy array
    #use biopython to count the seuquence length
    totBaseCnt = 0
    for record in SeqIO.parse(open(inSeq, 'rU'), 'fasta'):
        totBaseCnt += len(record.seq)
        tmpList.append(len(record.seq))
    lenValues = np.array(tmpList)
    del tmpList
    #let's return the stats
    # min, max, avg, std, cnt
    return(np.min(lenValues), np.max(lenValues), round(np.mean(lenValues), 4), round(np.std(lenValues), 4), len(lenValues), totBaseCnt)


def test_calcSeqStats(debug=False):
    """Test the function to check the input sequence stats."""
    #fasta
    fasta = '/user/gen-info/salvocos/test_directory/mod_vibrio/1d/c48_assembly_500bp.fasta'
    tpl = calcSeqStats(fasta)
    if debug:
        print(str(tpl))
