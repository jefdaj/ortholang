'''
This module contains different utility functions to get info on seuquence files
it also include some functions to format blast output
'''
from __future__ import print_function
import sys
#### IMPORT TO GENERATE PyPi package
from sonicparanoid import sys_tools as systools
####

#### IMPORTS TO RUN LOCALLY
#import sys_tools as systools
####

import os
import subprocess

__module_name__ = 'Sequence Tools'
__source__ = 'seq_tools.py'
__author__ = 'Salvatore Cosentino'
#__copyright__ = ''
__license__ = 'GPL'
__version__ = '1.7'
__maintainer__ = 'Cosentino Salvatore'
__email__ = 'salvo981@gmail.com'



#get the path to the directory where the scripts are stored
pySrcDir = os.path.dirname(os.path.abspath(__file__))
metapatfinder_root, d1 = pySrcDir.rsplit('/', 1)
metapatfinder_root += '/'
bin_root = '%sbin/'%metapatfinder_root


def info():
    """
    Thi module contains different utility functions to get info on seuquence files.

    it also include some functions to format blast output
    """
    print('MODULE NAME:\t%s'%__module_name__)
    print('SOURCE FILE NAME:\t%s'%__source__)
    print('MODULE VERSION:\t%s'%__version__)
    print('LICENSE:\t%s'%__license__)
    print('AUTHOR:\t%s'%__author__)
    print('EMAIL:\t%s'%__email__)



def checkFastaHdrForBlanks(inSeq, debug=False):
    """Check if the input fasta file has blanks in the any header."""
    if debug:
        print('checkFastaHdrForBlanks :: START')
        print('Input seq:\t%s'%inSeq)
    seqCnt = 0
    hasBlanks = False
    hasMultiGt = False # says if the header has multiple '>' symbols
    badHdr = None
    # start reading the file
    for ln in open(inSeq):
        if ln[0] == '>':
            seqCnt += 1
            tmpLn = ln[1:].rstrip('\n')
            if (' ' in tmpLn):
                hasBlanks = True
                badHdr = ln.rstrip('\n')
            if ('>' in tmpLn):
                hasMultiGt = True
                badHdr = ln.rstrip('\n')
            # exit the loop if one of the 2 happens
            if hasBlanks or hasMultiGt:
                break
        continue
    if seqCnt == 0:
        sys.stderr.write('\nERROR: the input file contain no valid FASTA headers. Please provide a valid FASTA file.\n')
        sys.exit(-5)
    if debug:
        print('Sequences in input FASTA:\t%d'%seqCnt)
    return (seqCnt, badHdr, hasBlanks, hasMultiGt)



def checkMoleculeType(inSeq, debug=True):
    """Check if the input fasta sequences are Proteins."""
    if debug:
        print('checkMoleculeType :: START')
        print('INPUT SEQ:\n%s'%inSeq)
    # parse the file using biopython
    from Bio import SeqIO
    cnt = wcnt = 0
    #cntList = []
    cumulativeSet = set()
    for record in SeqIO.parse(open(inSeq), "fasta"):
        cnt += 1
        seqLetters = set(str(record.seq).upper())
        # add the letters to the cumulative set
        cumulativeSet = cumulativeSet.union(seqLetters)
    # calculate the average number of letters in the sequences
    if len(cumulativeSet) < 15:
        sys.stderr.write('\nWARNING: the {:d} sequences for {:s} are composed of only the following {:d} symbols:'.format(cnt, os.path.basename(inSeq), len(cumulativeSet)))
        sys.stderr.write('\n{:s}\nPlease make sure that your FASTA files contain proteins and not DNA, nor RNA sequences.\n'.format(', '.join(str(s) for s in cumulativeSet)))
    elif len(cumulativeSet) > 29:
        sys.stderr.write('\nERROR: the {:d} sequences for {:s} contain too many ({:d}) symbols:'.format(cnt, os.path.basename(inSeq), len(cumulativeSet)))
        sys.stderr.write('\n{:s}\nPlease make sure that your FASTA files contain valid proteins.\n'.format(', '.join(str(s) for s in cumulativeSet)))
        sys.exit(-5)



def checkProtSeqIntegrity(inSeq, withExtraKeys=False, debug=True):
    """Check if the input fasta protein sequence is valid."""
    if debug:
        print('checkProtSeqIntegrity :: START')
        print('Input seq:\n%s'%inSeq)
        print('Contains extra keys:\n%s'%str(withExtraKeys))
    #first of all check that the input sequence is a fasta
    fmtTpl = checkSeqFormat(inSeq, debug)
    fmt = fmtTpl[1]
    if fmt != 'fasta':
        sys.stderr.write('\nERROR: The file %s\n must be FASTA format\n'%inSeq)
        sys.exit(-4)
    stdKeys = ['a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k', 'l', 'm', 'n', 'o', 'p', 'q', 'r', 's', 't', 'u', 'v', 'w', 'y', 'z']
    extraKeys = ['x', '*', '-']
    from collections import OrderedDict
    stdKeysDict = OrderedDict()
    for el in stdKeys:
        stdKeysDict[el] = None
    #add the extra keys if reuiqred
    if withExtraKeys:
        for el in extraKeys:
            stdKeysDict[el] = None
    if debug:
        print('The reference symbols dictionary contains %s elements.'%len(stdKeysDict))
    #extract the sequences using biopython
    from Bio import SeqIO
    for record in SeqIO.parse(open(inSeq, 'rU'), fmt):
        seq = str(record.seq)
        seq = seq.lower()
        seqId = record.id
        #print(seq)
        for idx, val in enumerate(seq):
            if not val in stdKeysDict:
                sys.stderr.write('\nInvalid aminoacid "%s" in seq:%s position:%s'%(val, seqId, idx))
                return False
    #everything looks fine!
    return True



def chopSeqFile(inSeq, outDir=os.getcwd(), chunks=1, prefix=None, noFmtCheck=False, debug=False):
    """Chop input sequence in N files containing almost the same number of sequences."""
    if debug:
        print('chopSeqFile :: START')
        print('Input seq:\t%s'%inSeq)
        print('Output dir:\t%s'%outDir)
        print('Chunks:\t%s'%str(chunks))
        print('Prefix:\t%s'%prefix)
        print('Skip input format check:\t%s'%noFmtCheck)
    #add final backslash if missing
    if outDir[-1] != '/':
        outDir += '/'
    #first of all check that the input sequence is a fasta
    fmtTpl = fmt = None
    if not noFmtCheck:
        fmtTpl = checkSeqFormat(inSeq, debug)
        fmt = fmtTpl[1]
        if fmt != 'fasta':
            sys.stderr.write('\nERROR: The file %s\n must be FASTA format\n'%inSeq)
            sys.exit(-4)
    #Count sequences
    #USE THE FUNCTION ALREADY CREATED!
    faHdrCnt = 0#counter for fasta headers
    lnCnt = 0#line counter
    #let's open the input file and start counting
    ifd = open(inSeq)
    for ln in ifd:
        if ln.strip() == '': #empty line
            continue
        lnCnt += 1
        if ln[0] == '>':
            faHdrCnt += 1
    #Calculate files to be created
    if chunks >= faHdrCnt:
        sys.stderr.write('\nERROR: the number of chunks (%d) must be lower than the number of input sequences (%d)\n'%(chunks, faHdrCnt))
        sys.exit(-5)
    if chunks <= 1:
        sys.stderr.write('\nERROR: the number of chunks (%d) must be higher than 1\n'%(chunks))
        sys.exit(-5)
    #calculate the number of files to be created
    fsize = int(faHdrCnt/chunks)
    fnum = chunks
    extraFsize = faHdrCnt%chunks #size of the extra file to be created in case modulo is different from 0
    if extraFsize != 0:
        fnum += 1
        sys.stderr.write('\nWarning: and extra file containing %d sequences will be created\n'%extraFsize)
    if debug:
        print('The input file contains %d sequences.'%faHdrCnt)
        print('Each chunk will contain %d sequences.'%fsize)
        if extraFsize != 0:
            print('An extra chunk containing %d sequences will be created.'%extraFsize)
        print('in total %d files will be created.'%fnum)
    #set the prefix if required
    bName = os.path.basename(inSeq)
    #take remove the file format
    bName = bName.rsplit('.', 1)[0]
    chunkList = [] #will contain the paths to the output files
    for i in range(1, fnum + 1):
        if prefix is None:
            if fmt is None:
                chunkList.append('%s%s_chunk%d'%(outDir, bName, i))
            else: #add the file format
                chunkList.append('%s%s_chunk%d.%s'%(outDir, bName, i, fmt))
        else:
            if fmt is None:
                chunkList.append('%s%s_%d.%s'%(outDir, prefix.strip(), i))
            else:
                chunkList.append('%s%s_%d.%s'%(outDir, prefix.strip(), i, fmt))
    #print the chunks list
    if debug:
        print('The following chunks will be created:')
        for el in chunkList:
            print(el)
    #open the first file
    currentOutFile = chunkList[0]
    writingLastChunk = False
    tmpOfd = open(currentOutFile, 'w')
    from Bio import SeqIO
    cnt = wcnt = 0
    for record in SeqIO.parse(open(inSeq), 'fasta'):
        cnt += 1
        seq = str(record.seq)
        #seq = seq.lower()
        seqId = record.id
        #write in the output file
        tmpOfd.write('>%s\n'%(seqId))
        tmpOfd.write('%s\n'%(seq))
        wcnt += 1
        #check if the size limit has been reached
        if (wcnt == fsize):
            if writingLastChunk:
                continue
            currentOutFileIdx = chunkList.index(currentOutFile)
            tmpOfd.close()
            #use the next file descriptor
            currentOutFile = chunkList[currentOutFileIdx + 1]
            #avoid index out of bound for file paths
            if currentOutFileIdx + 1 == len(chunkList) - 1:
                writingLastChunk = True
            tmpOfd =  open(currentOutFile, 'w')
            wcnt = 0
    #return a list with the created files, the standard chunk size and extra file chunk size
    return(chunkList, fsize, extraFsize)



def countSeqs(inSeq, debug=False):
    """Count sequences in the input fasta file."""
    if debug:
        print('\ncountSeqs START:')
        print('INPUT SEQ ::\t%s'%inSeq)
    #check the special case of protein fasta file extension
    sFormat = 'fasta'
    #check that there file is not empty
    stInfo = os.stat(inSeq)
    if stInfo.st_size == 0:
        sys.stderr.write('\nWARNING: the input file is empty, 0 will be returned.\n')
        return 0
    faHdrCnt = 0#counter for fasta headers
    lnCnt = 0#line counter
    #let's open the input file and start counting
    ifd = open(inSeq)
    for ln in ifd:
        if len(ln.strip()) == 0: #empty line
            continue
        lnCnt += 1
        if ln[0] == '>':
            faHdrCnt += 1
    ifd.close()
    #return the count
    return faHdrCnt



def fltrSeqByLen(inSeq, outDir=None, minLen=100, debug=True):
    """filter a fasta sequence file based on the minimum length given as input parameter."""
    from Bio import SeqIO
    if debug:
        print('fltrSeqByLen :: START')
        print('INPUT SEQ:\n%s'%inSeq)
    #check the existence of the input file
    if not os.path.isfile(inSeq):
        sys.stderr.write('The file %s was not found, please provide a valid file path'%inSeq)
        sys.exit(-2)
    #check the existence of the output directory
    if outDir != None:
        if not os.path.isdir(outDir):
            sys.stderr.write('The directory %s was not found, please provide the path to a valid directory'%outDir)
            sys.exit(-2)
    else:
        outDir = os.path.dirname(inSeq) + '/'
    #set output file name
    if minLen < 50:
        minLen = 50
    outName = os.path.basename(inSeq)
    flds = outName.split('.')
    outName = '.'.join(flds[0:len(flds)-1]) + '.%sbp.'%str(minLen) + flds[-1]
    outPath = outDir + outName
    if debug:
        print('OUTPUT SEQ:\n%s'%outPath)
    #open output file
    fdOut = open(outPath, 'w')
    totSeq = 0
    removedSeqCnt = 0
    for seq_record in SeqIO.parse(open(inSeq), 'fasta'):
        totSeq = totSeq + 1
        if len(seq_record) < minLen:
            removedSeqCnt = removedSeqCnt + 1
        else:
            SeqIO.write(seq_record, fdOut, 'fasta')
    fdOut.close()
    if debug:
        print('Sequences:\t%d'%totSeq)
        print('Filtered (shorter than %d):\t%d'%(minLen, removedSeqCnt))
        print('Remaining Seqs:\t%d'%(totSeq-removedSeqCnt))
    #return a tuple with the total number of sequences and removed sequences
    return (outPath, totSeq, removedSeqCnt)



def formatFastaHdr(inSeq, newSymbol='|', debug=False):
    """Format the headers by substituting blanks with the desired symbols."""
    if debug:
        print('formatFastaHdr :: START')
        print('Input seq:\t%s'%inSeq)
    outName = os.path.basename(inSeq)
    outName = '%s_no_blanks'%(outName.split('.', 1)[0])
    outPath = '%s/%s'%(os.path.dirname(inSeq), outName)
    ofd = open(outPath, 'w')
    # start reading the input file and write the output file
    for ln in open(inSeq):
        if ln[0] == '>':
            # substitute spaces with the new symbol
            newLn = newSymbol.join(ln[1:].split(' '))
            # replace any extra '>' symbols
            newLn = newLn.replace('>', newSymbol)
            # replace Tabs if any...
            newLn = newLn.replace('\t', newSymbol)
            ofd.write('>%s'%newLn)
            # old version
            #ofd.write(newSymbol.join(ln.split(' ')))
        else:
            # if not empty or not newline, write the line without making any change
            if len(ln) > 1:
                ofd.write(ln)
    ofd.close()
    if debug:
        print('Output path:\t%s'%outPath)
    return (outPath, newSymbol)



def test_checkFastaHdrForBlanks(debug=True):
    '''Checks if the input sequence contains blanks.'''
    inSeq = '/Users/salvocos/Desktop/luteoviolacea_ATCC33492'
    #check sequence
    print(checkFastaHdrForBlanks(inSeq, debug=debug))



def test_checkMoleculeType(debug=True):
    '''Checks if the input seq is DNA, PROTEIN OR RNA.'''
    inDna = '/user/gen-info/salvocos/projects/pathogenFinder2/gold_data/test_gbk_conversion/fasta/209910450_ext.fasta'
    inProt = '/user/gen-info/salvocos/projects/pathogenFinder2/gold_data/test_gbk_conversion/fasta/215267879_prot_id_bprj.faa'
    #check dna
    print(checkMoleculeType(inDna, debug))
    #check protein fasta
    print(checkMoleculeType(inProt, debug))



def test_countSeqs(debug=True):
    '''Test count sequences from fasta or fastq.'''
    outTestDir = '/user/gen-info/salvocos/tmp/test_seq_tools/'
    #fasta = 'input.fasta'
    fasta = '%sinput/30407157.faa'%outTestDir
    cnt = countSeqs(fasta, debug=debug)
    if debug:
        print(str(cnt))



def test_fltrSeqByLen(debug=True):
    '''Test the function to filter sequence files.'''
    fasta = '/auto/user/gen-info/salvocos/test_directory/mod_vibrio/1d/c48_assembly_500bp.fasta'
    tpl = fltrSeqByLen(fasta, None, 550, debug)
    print(str(tpl))



def test_formatFastaHdr(debug=True):
    '''Checks if the input sequence contains blanks.'''
    inSeq = '/Users/salvocos/Desktop/luteoviolacea_ATCC33492'
    # reformat headers
    outPath = formatFastaHdr(inSeq, newSymbol='¥', debug=debug)
    print(outPath)
