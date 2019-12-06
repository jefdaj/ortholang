'''Remove paralogs with high difference in length from their main ortholog gene.'''
import os
import sys
from collections import OrderedDict
#### IMPORT TO GENERATE PyPi package
from sonicparanoid import sys_tools as systools
####

#### IMPORTS TO RUN LOCALLY
#import sys_tools as systools
####

__module_name__ = 'Ortholog detection'
__source__ = 'length_difference_filter.py'
__author__ = 'Salvatore Cosentino'
#__copyright__ = ''
__license__ = 'GPL'
__version__ = '0.1'
__maintainer__ = 'Cosentino Salvatore'
__email__ = 'salvo981@gmail.com'



### FUNCTIONS ####

def info():
    """This module contains functions for the detection of orthologs."""
    print('MODULE NAME:\t%s'%__module_name__)
    print('SOURCE FILE NAME:\t%s'%__source__)
    print('MODULE VERSION:\t%s'%__version__)
    print('LICENSE:\t%s'%__license__)
    print('AUTHOR:\t%s'%__author__)
    print('EMAIL:\t%s'%__email__)



def calc_longest_ortho(orthologsList, protLenDict, debug=False):
    '''Calculates the length of longest ortholog from those in the input list.'''
    if debug:
        print('\ncalc_longest_ortho :: START')
        print('Orthologs list length: %s'%(len(orthologsList)))
        #print('Orthologs: %s'%(orthologsList))
        print('Proteins in dictionary: %s'%(len(protLenDict)))

    # check that the list as at least 2 elements
    if len(orthologsList) < 2:
        sys.exit('ERROR: the ortholog list must contain at least 2 proteins')
    if len(orthologsList) > 3:
        sys.exit('DEBUG: found entry with more than 3 orthologs!')
    # final length
    maxLen = 0
    shortestLen = 0
    # start looping the list
    for gene in orthologsList:
        tmpLen = int(protLenDict[gene])
        print(tmpLen)
        if tmpLen == 0:
            sys.exit('No ortholog can be of length 0!')
        if tmpLen > maxLen:
            if shortestLen == 0:
                shortestLen = tmpLen
            if debug:
                print('Max len updated: %d -> %d'%(maxLen, tmpLen))
            maxLen = tmpLen
    # calculate lenght difference between shortest and longest
    if shortestLen == 0:
        shortestLen = maxLen
    maxLenDiff = round(float(shortestLen)/float(maxLen), 3)
    maxLenDiff = round(abs(maxLenDiff - 1), 3)
    #print(maxLenDiff)
    #sys.exit('DEBUG')
    if maxLenDiff >= 1.0:
        print('WARNING: pairs of orthologs should not have a length diffence higher than 20%')
        print('Length difference in %%:\t%s'%str(maxLenDiff))
        print(orthologsList)
        sys.exit(-5)
    if len(orthologsList) > 3:
        print(orthologsList)
        sys.exit('DEBUG')
    # return the maximum length
    return maxLen



# TODO remove maxLenDiff, maxLen, and minLen. Implement in Cython
def calc_ortholog_leghths(orthologsList, protLenDict, debug=False):
    '''
    Extract ortholog lengths for the genes in the input list.

    If a length is present more than one time than it is added only one time.
    '''
    if debug:
        print('\ncalc_ortholog_leghths :: START')
        print('Orthologs list length: %s'%(len(orthologsList)))
        print('Proteins in dictionary: %s'%(len(protLenDict)))
    # check that the list as at least 2 elements
    if len(orthologsList) < 2:
        sys.exit('ERROR: the ortholog list must contain at least 2 proteins')
    # final length
    maxLen = 0
    shortestLen = 0
    maxLenDiff = 0
    # dictionary to store the lengths
    lenDict = {}
    # start looping the list
    for gene in orthologsList:
        tmpLen = int(protLenDict[gene])
        if tmpLen == 0:
            sys.exit('No ortholog can be of length 0!')
        if not tmpLen in lenDict:
            lenDict[tmpLen] = None
        # remove the key from dictionary with lengths
        # sometimes the same ortholog appear in more than one cluster
        # after that bug is removed the dictionary can be dinamycally shortened
        #del protLenDict[gene]

    # generate the output list
    outList = list(lenDict.keys())
    if len(outList) == 1: # max and min len have the same value
        shortestLen = maxLen = outList[0]
        maxLenDiff = 0
    else:
        outList.sort()
        # calculate lenght difference between shortest and longest
        shortestLen = outList[0]
        maxLen = outList[-1]
        maxLenDiff = round(float(shortestLen)/float(maxLen), 3)
        maxLenDiff = round(abs(maxLenDiff - 1), 3)
    if maxLenDiff >= 1.0:
        print('WARNING: pairs of orthologs should not have a length diffence higher than 20%')
        sys.exit(-5)
    #if len(orthologsList) > 16:
        #print(orthologsList)
        #print('Lengths: %s'%(', '.join(map(str, outList))))
        #sys.exit('DEBUG :: many orthologs!')
    if debug:
        print('Lengths: %s'%(', '.join(map(str, outList))))
        print('Longest ortholog:\t%d'%maxLen)
        print('Shortest ortholog:\t%d'%shortestLen)
        print('Max length difference:\t%s'%str(maxLenDiff))
    # return list with ortholog lengths
    return outList



def extract_paralogs(clstrSubstr, debug=False):
    '''Parse a cluster substring and return Orhtologs and Inparalogs.'''
    if debug:
        print('\nextract_paralogs :: START')
        print('Cluster substring: %s'%(repr(clstrSubstr)))
    # orthologs list
    orthoDict = OrderedDict()
    inparaDict = OrderedDict()
    # start analyzing the substring
    gene = None
    clstrParts = clstrSubstr.split(' ')
    if len(clstrParts)%2 != 0:
        #print(repr(clstrSubstr))
        sys.exit('ERROR: the split must give an even number of parts.')
    for i, el in enumerate(clstrParts):
        # then it is the paralogs
        if i%2 == 0:
            # add the gene to dictionary
            gene = el
            continue
        else: # check if it is a ortholog or an inparalog
            #score = float(el)
            #NOTE this rounding must be avoided in later developments
            score = round(float(el), 2)
            # then it is a core ortholog
            if score == 1.0:
                # this should not be required
                if not gene in orthoDict:
                    orthoDict[gene] = None
                else:
                    sys.exit('ERROR: repeated ortholog gene!')
            elif score > 1.:
                sys.exit('ERROR: This cannot be!')
            else: # it is an inparalog
                # inparalog in dicitonary
                inparaDict[gene] = score
    return(list(orthoDict.keys()), inparaDict)


# NOTE this is not working because is not always the case that the first is the main ortholog
def extract_paralogs_single_core(clstrSubstr, debug=False):
    '''
    Parse a cluster substring and return Orhtologs and Inparalogs.

    This function assumes that the first gene with 1.0 is the one and only
    core ortholog in the clusters for species X.
    I will only return one ortholog, and different paralogs.

    THIS IS POTENTIALLY WRONG!!!
    '''
    if debug:
        print('\nextract_paralogs_single_core :: START')
        print('Cluster substring: %s'%(repr(clstrSubstr)))
    # orthologs list
    orthoList = []
    inparaDict = OrderedDict()
    # start analyzing the substring
    gene = None
    clstrParts = clstrSubstr.split(' ')
    # extract the remaining genes
    if len(clstrParts)%2 != 0:
        #print(repr(clstrSubstr))
        sys.exit('ERROR: the split must give an even number of parts.')
    for i, el in enumerate(clstrParts):
        # then it is the paralogs
        if i%2 == 0:
            # add the gene to dictionary
            gene = el
            continue
        else: # check if it is a ortholog or an inparalog
            score = float(el)
            #NOTE this creates a higher number of CORE orthologs
            #score = round(float(el), 2)
            # then it is a core ortholog
            if score == 1.:
                if len(orthoList) == 0:
                    orthoList.append(gene)
                else: # add it to the inparalog dictionary
                    inparaDict[gene] = score
            elif score > 1.:
                sys.exit('ERROR: This cannot be!')
            else: # it is an inparalog with score < 1.0
                # inparalog in dictionary
                inparaDict[gene] = score
    return(orthoList, inparaDict)



def load_seq_lengths(proteome, debug=False):
    '''Load the length of each protein sequence in the input proteome.'''
    if debug:
        print('load_seq_lengths :: START')
        print('Proteome: %s'%proteome)
    if not os.path.isfile(proteome):
        sys.exit('ERROR: the proteome file\n%s\ndoes not exist!\n'%(proteome))
    lenDict = OrderedDict()
    # use biopython
    from Bio import SeqIO
    for seq_record in SeqIO.parse(open(proteome), 'fasta'):
        recId = seq_record.id
        if not recId in lenDict:
            lenDict[recId] = int(len(seq_record))
    if debug:
        print('Loaded sequences for %s:\t%d'%(os.path.basename(proteome), len(lenDict)))
    return lenDict



#TODO remove test from the code
#TODO ad read and writing counts as debug
def filter_ortholog_table(abTbl, a, b, outDir=os.getcwd(), lenThr=0.25, debug=False):
    '''Filter ortholog table based on sequence legths.'''
    if debug:
        print('filter_ortholog_table :: START')
        print('Ortholog table: %s'%abTbl)
        print('Proteome A: %s'%a)
        print('Proteome B: %s'%b)
        print('Output directory: %s'%outDir)
        print('Length difference threshold: %s'%str(lenThr))
    # load sequence lengths for A and B
    lenDictA = load_seq_lengths(a, debug)
    lenDictB = load_seq_lengths(b, debug)

    # load the information from a clusters
    # EXAMPLE
    #OrtoId	Score	OrtoA	OrtoB
    #1	1163	1423_Q9KWU4 1.0	9606_P11498 1.0
    #2	963	1423_P09339 1.0	9606_P21399 1.0 9606_P48200 0.201

    # these will contain info for paralogs for each of the 2 species
    if outDir[-1] != '/':
        outDir = '%s/'%outDir

    # check that the output directory is different from the one of the input file
    inDir = '%s/'%os.path.dirname(abTbl)
    if inDir == outDir:
        sys.exit('The output directory must be different from that of the input table.')
    # create output directory if needed
    systools.makedir(outDir)

    # new table path
    abTblNew = '%s%s'%(outDir, os.path.basename(abTbl))
    # rejected list file path
    rjctTbl = '%s%s'%(outDir, os.path.basename(abTbl.replace('table.', 'rejected.')))
    # new table path
    abTblNew = '%s%s'%(outDir, os.path.basename(abTbl))
    # rejected list file path
    rjctTbl = '%s%s'%(outDir, os.path.basename(abTbl.replace('table.', 'rejected.')))
    # open output files
    ofdNewTbl = open(abTblNew, 'w')
    ofdRjct = open(rjctTbl, 'w')

    # count read and wrote genes
    orthoRdCntA = inparaRdCntA = orthoRdCntB = inparaRdCntB = 0
    orthoWrtCntA = inparaWrtCntA = orthoWrtCntB = inparaWrtCntB = 0

    for ln in open(abTbl):
        if ln[0] == 'O':
            ofdNewTbl.write(ln)
            continue
        ln = ln.rstrip('\n')
        clstrId, score, paraA, paraB = ln.split('\t')

        # extract orthologs and inparalogs from A
        orthoListA, inparaDictRawA = extract_paralogs(paraA, debug=debug)
        orthoRdCntA += len(orthoListA)
        inparaRdCntA += len(inparaDictRawA)

        # If there are InParalogs then check if they should be kept or not
        keptInpaListA = []
        droppedInpaDictA = OrderedDict()
        if len(inparaDictRawA):
            #print('ClstrID:\t%s'%clstrId)
            # set ortholog length for A
            lenListOrthoA = []
            if len(orthoListA) > 1:
                lenListOrthoA = calc_ortholog_leghths(orthoListA, lenDictA, debug=debug)
            elif len(orthoListA) == 0:
                sys.exit('ERROR: at least one ortholog must be found!')
            else:
                # add the only available length
                lenListOrthoA.append(lenDictA[orthoListA[0]])
            # filter Inparalogs from A
            droppedInpaDictA, keptInpaListA = filter_inparalogs(inparaDictRawA, lenDictA, orthoLenList=lenListOrthoA, lenRatioThr=lenThr, debug=debug)

        '''
        if clstrId == '777':
            print(orthoListA)
            print(inparaDictRawA)
            print(lenListOrthoA)
            print(droppedInpaDictA)
            print(keptInpaListA)
            sys.exit('Test single cluster')
        #'''

        # extract orthologs and inparalogs from B
        orthoListB, inparaDictRawB = extract_paralogs(paraB, debug=debug)
        orthoRdCntB += len(orthoListB)
        inparaRdCntB += len(inparaDictRawB)

        # If there are InParalogs then check if they should be kept or not
        keptInpaListB = []
        droppedInpaDictB = OrderedDict()
        if len(inparaDictRawB):
            # set ortholog length for B
            lenListOrthoB = []
            if len(orthoListB) > 1:
                lenListOrthoB = calc_ortholog_leghths(orthoListB, lenDictB, debug=debug)
            elif len(orthoListB) == 0:
                sys.exit('ERROR: at least one ortholog must be found!')
            else:
                # add the only available length
                lenListOrthoB.append(lenDictB[orthoListB[0]])
            # filter Inparalogs from B
            droppedInpaDictB, keptInpaListB = filter_inparalogs(inparaDictRawB, lenDictB, orthoLenList=lenListOrthoB, lenRatioThr=lenThr, debug=debug)

        # START WRITING THE NEW TABLE
        ofdNewTbl.write('%s\t%s\t'%(clstrId, score))

        # Write the output cluster for A
        # Write cores orthologs for A
        tmpLnList = []
        for orthoTmpGene in orthoListA:
            tmpLnList.append('%s 1.0'%(orthoTmpGene))
        ofdNewTbl.write(' '.join(tmpLnList))
        orthoWrtCntA += len(tmpLnList)

        # Write rejected inparalogs
        for k in droppedInpaDictA:
            tmpLenDiff, tmpConf, inparaVsOrthoRatio = droppedInpaDictA[k]
            ofdRjct.write('%s\t%s\t%s\t%s\t%s\t%s\n'%(clstrId, score, k, tmpConf, tmpLenDiff, inparaVsOrthoRatio))
        # reset tmp list
        tmpLnList.clear()
        # write valid inparalogs to cluster
        for tmpInparaA in keptInpaListA:
            tmpLnList.append('%s %s'%(tmpInparaA, inparaDictRawA[tmpInparaA]))
        if len(tmpLnList) > 0:
            ofdNewTbl.write(' %s'%(' '.join(tmpLnList)))
        inparaWrtCntA += len(tmpLnList)
        tmpLnList.clear()

        # now start writing the right part of the cluster
        ofdNewTbl.write('\t')

        # Write core orthologs for B
        tmpLnList.clear()
        for orthoTmpGene in orthoListB:
            tmpLnList.append('%s 1.0'%(orthoTmpGene))
        ofdNewTbl.write(' '.join(tmpLnList))
        orthoWrtCntB += len(tmpLnList)

        # Write rejected inparalogs
        for k in droppedInpaDictB:
            tmpLenDiff, tmpConf, inparaVsOrthoRatio = droppedInpaDictB[k]
            ofdRjct.write('%s\t%s\t%s\t%s\t%s\t%s\n'%(clstrId, score, k, tmpConf, tmpLenDiff, inparaVsOrthoRatio))

        # reset tmp list
        tmpLnList.clear()
        # write valid inparalogs to cluster
        for tmpInparaB in keptInpaListB:
            tmpLnList.append('%s %s'%(tmpInparaB, inparaDictRawB[tmpInparaB]))
        if len(tmpLnList) > 0:
            ofdNewTbl.write(' %s'%(' '.join(tmpLnList)))
        inparaWrtCntB += len(tmpLnList)
        tmpLnList.clear()

        # close the cluster line
        ofdNewTbl.write('\n')

        ###### TEST #########
        '''
        # Try to rewrite the table as it was originally
        ofdNewTbl.write('%s\t%s\t'%(clstrId, score))
        tmpLnList = []
        # write cores orthologs for A
        for orthoTmpGene in orthoListA:
            tmpLnList.append('%s 1.0'%(orthoTmpGene))
        ofdNewTbl.write(' '.join(tmpLnList))
        orthoWrtCntA += len(tmpLnList)
        # reset tmp list
        tmpLnList.clear()
        for k in inparaDictRawA:
            tmpLnList.append('%s %s'%(k, inparaDictRawA[k]))
        if len(tmpLnList) > 0:
            ofdNewTbl.write(' %s'%(' '.join(tmpLnList)))
        inparaWrtCntA += len(tmpLnList)
        tmpLnList.clear()
        #'''

        '''
        # now start with the right part of the cluster
        ofdNewTbl.write('\t')
        # write the orthologs for B
        for orthoTmpGene in orthoListB:
            tmpLnList.append('%s 1.0'%(orthoTmpGene))
        ofdNewTbl.write(' '.join(tmpLnList))
        orthoWrtCntB += len(tmpLnList)
        # reset tmp list
        tmpLnList.clear()
        # write InParalogs
        for k in inparaDictRawB:
            tmpLnList.append('%s %s'%(k, inparaDictRawB[k]))
        if len(tmpLnList) > 0:
            ofdNewTbl.write(' %s'%(' '.join(tmpLnList)))
        inparaWrtCntB += len(tmpLnList)
        ofdNewTbl.write('\n')
        ######################
        #'''

    # close output files
    ofdNewTbl.close()
    ofdRjct.close()

    if debug:
        print('\nRead orthologs A/B; inparalogs A/B:')
        print('%d\t%d\t%d\t%d'%(orthoRdCntA, orthoRdCntB, inparaRdCntA, inparaRdCntB))
        print('\nWritten orthologs A/B; inparalogs A/B:')
        print('%d\t%d\t%d\t%d'%(orthoWrtCntA, orthoWrtCntB, inparaWrtCntA, inparaWrtCntB))



def filter_inparalogs(dropCandidates, proteinLenDict, orthoLenList=[], lenRatioThr=0.25, debug=False):
    '''
    Check that the inparalog lengths are within a threshold lengths.

    Return inparalogs to be removed and those that should be kept
    '''
    if debug:
        print('\nfilter_inparalogs :: START')
        print('Inparalogs to be tested: %s'%str(dropCandidates))
        print('Proteins lengths dictionary: %s'%(len(proteinLenDict)))
        print('List of ortholog lengths to use as reference: %s'%(str(orthoLenList)))
        print('Length difference threshold: %s'%(str(lenRatioThr)))
    # lenRatioThr = 0.25
    # it means that by default the inparalog must have a length difference below 25% with the ortholog
    if orthoLenList == 0:
        sys.exit('ERROR: you must provide at least one ortholog length.')

    # dictionary with inparalogs to be removed
    dropDict = OrderedDict()
    # list of inparalogs to be kept
    keepList = []
    # use all the ortholog lengths to decide if a inparalog should be kept or dropped
    for inparaGene in dropCandidates:
        #print('\nTesting inparalog:\t%s'%inparaGene)
        tmpConf = dropCandidates[inparaGene]
        # extract inparalog length
        tmpInparaLen = proteinLenDict[inparaGene]
        # counter to decide if the inparalog should be dropped or not
        # if counter is 0 then the length differences between the inparalog
        # and all the other orthologs in the clusters are too high,
        # hence it should be dropped
        keepCnt = 0
        lenDiff = 0
        # list to contain the length differences (only one difference will returned)
        #lenDiffList = []
        # sets to store length differences
        lenDiffNegSet = set()
        lenDiffPosSet = set()
        inparaLenRatioSet = set()
        # compare the inparalog lenght to that of all the ortholog lengths
        for ol in orthoLenList:
            # test if the length conditions are satisfied
            keepInpara, lenDiff, inparaLenRatio = length_difference_check(ol, tmpInparaLen, lenRatioThr, debug=debug)
            keepCnt += keepInpara
            inparaLenRatioSet.add(inparaLenRatio)
            if lenDiff < 0:
                lenDiffNegSet.add(lenDiff)
            else:
                lenDiffPosSet.add(lenDiff)
        if keepCnt == 0:
            #print('Keep counter DROP:\t%d'%keepCnt)
            #print(lenDiffList)
            #print('To drop gene/length/length difference:\t%s\t%s\t%s'%(inparaGene, tmpInparaLen, lenDiff))

            if len(orthoLenList) > 2:
                # find the minimum distance
                if len(lenDiffNegSet) == 0:
                    lenDiff = min(lenDiffPosSet)
                elif len(lenDiffPosSet) == 0:
                    lenDiff = max(lenDiffNegSet)
                else: # we need to find the closest to 0
                    if -max(lenDiffNegSet) < min(lenDiffPosSet):
                        lenDiff = max(lenDiffNegSet)
                    else:
                        lenDiff = min(lenDiffPosSet)
                #print(lenDiff)
                #print('positive/negative Sets:\t%s\t%s'%(lenDiffPosSet, lenDiffNegSet))
            # add the inparalog the distance difference to the Drop dictionary
            dropDict[inparaGene] = (lenDiff, tmpConf, max(inparaLenRatioSet))
            #print(lenDiff)
            #print('positive/negative Sets:\t%s\t%s'%(lenDiffPosSet, lenDiffNegSet))
        else: # keep the inparalog
            keepList.append(inparaGene)
            #print('Keep counter KEEP:\t%d'%keepCnt)
            #print('Lenght difference:\t%s'%lenDiff)

    if len(orthoLenList) > 8:
        print(keepList)
        print(dropDict)
        sys.exit('DEBUG: many ortholog lengths to compare')
    return(dropDict, keepList)



def filter_inparalogs_single_core(dropCandidates, proteinLenDict, orthoLen=100, lenRatioThr=0.25, debug=False):
    '''
    Check that the inparalog and CORE gene lengths are within a threshold lengths.

    Return paralogs to be removed and those that should be kept.
    This function assumes that a single main ortholog is in the cluster
    hence, the paralogs are compared to a single length.
    '''
    if debug:
        print('\nfilter_inparalogs_single_core :: START')
        print('Inparalogs to be tested: %s'%str(dropCandidates))
        print('Proteins lengths dictionary: %s'%(len(proteinLenDict)))
        print('Main ortholog gene length to use as reference: %d'%(orthoLen))
        print('Length difference threshold: %s'%(str(lenRatioThr)))
    # it means that by default the inparalog must have a length difference below 25% with the ortholog
    if orthoLen < 0:
        sys.exit('ERROR: the ortholog length should be higher than 10 residues.')

    # dictionary with inparalogs to be removed
    dropDict = OrderedDict()
    # list of inparalogs to be kept
    keepList = []
    # use all the ortholog lengths to decide if a inparalog should be kept or dropped
    for inparaGene in dropCandidates:
        #print('\nTesting inparalog:\t%s'%inparaGene)
        tmpConf = dropCandidates[inparaGene]
        # extract inparalog length
        tmpInparaLen = proteinLenDict[inparaGene]
        # counter to decide if the inparalog should be dropped or not
        # if counter is 0 then the length differences between the inparalog
        # and all the other orthologs in the clusters are too high,
        # hence it should be dropped
        keepCnt = 0
        lenDiff = 0
        # list to contain the length differences (only one difference will returned)
        #lenDiffList = []
        # sets to store length differences
        lenDiffNegSet = set()
        lenDiffPosSet = set()
        inparaLenRatioSet = set()

        # compare the inparalog lenght to that of all the ortholog lengths
        for ol in orthoLenList:
            # test if the length conditions are satisfied
            keepInpara, lenDiff, inparaLenRatio = length_difference_check(ol, tmpInparaLen, lenRatioThr, debug=debug)
            keepCnt += keepInpara
            inparaLenRatioSet.add(inparaLenRatio)
            if lenDiff < 0:
                lenDiffNegSet.add(lenDiff)
            else:
                lenDiffPosSet.add(lenDiff)
        if keepCnt == 0:
            #print('Keep counter DROP:\t%d'%keepCnt)
            #print(lenDiffList)
            #print('To drop gene/length/length difference:\t%s\t%s\t%s'%(inparaGene, tmpInparaLen, lenDiff))

            if len(orthoLenList) > 2:
                # find the minimum distance
                if len(lenDiffNegSet) == 0:
                    lenDiff = min(lenDiffPosSet)
                elif len(lenDiffPosSet) == 0:
                    lenDiff = max(lenDiffNegSet)
                else: # we need to find the closest to 0
                    if -max(lenDiffNegSet) < min(lenDiffPosSet):
                        lenDiff = max(lenDiffNegSet)
                    else:
                        lenDiff = min(lenDiffPosSet)
                #print(lenDiff)
                #print('positive/negative Sets:\t%s\t%s'%(lenDiffPosSet, lenDiffNegSet))
            # add the inparalog the distance difference to the Drop dictionary
            dropDict[inparaGene] = (lenDiff, tmpConf, max(inparaLenRatioSet))
            #print(lenDiff)
            #print('positive/negative Sets:\t%s\t%s'%(lenDiffPosSet, lenDiffNegSet))
        else: # keep the inparalog
            keepList.append(inparaGene)
            #print('Keep counter KEEP:\t%d'%keepCnt)
            #print('Lenght difference:\t%s'%lenDiff)

    if len(orthoLenList) > 8:
        print(keepList)
        print(dropDict)
        sys.exit('DEBUG: many ortholog lengths to compare')
    return(dropDict, keepList)



def length_difference_check(orthologLen, inparalogLen, lenRatioThr, debug=False):
    '''
    Check the difference in length between a inparalong and its ortholog sequence (from the same species).

    Return a tuple of type (1, 0.4) where 1 means the inparalog should be kept, and 0.4 is
    the difference in length from the closest ortholog in the cluster.
    If the inparalog should be dropped then the first value is 0
    '''
    if debug:
        print('\nlength_difference_check :: START')
        print('Ortholog length: %d'%orthologLen)
        print('Inparalogs length: %d'%inparalogLen)
        print('Length difference threshold: %s'%(str(lenRatioThr)))
    # lenght difference between the two sequences
    lenDiffRatio = 0
    # simply return that it should be kept
    if orthologLen == inparalogLen:
        return(1, 0., 1.)
    else:
        # how long the inpralog is compared to the ortholog
        # this should be a negative multiplier if ortholog is longer
        inparaVsOrthoLen = 1.
        #inparaVsOrthoLen = round(float(inparalogLen)/float(orthologLen), 2)
        # consider which seq is the longest:
        if inparalogLen < orthologLen:
            seqLenDiff = orthologLen - inparalogLen
            lenDiffRatio = round(float(seqLenDiff)/float(orthologLen), 2)
            inparaVsOrthoLen = -round(float(orthologLen)/float(inparalogLen), 2)
        else: # the inparalog sequence is longer
            seqLenDiff = inparalogLen - orthologLen
            lenDiffRatio = round(float(seqLenDiff)/float(inparalogLen), 2)
            inparaVsOrthoLen = round(float(inparalogLen)/float(orthologLen), 2)
        # check if it should bet kept or rejected
        #if (-lenRatioThr <= lenDiffRatio <= lenRatioThr): # keep it
        if (lenDiffRatio <= lenRatioThr): # keep it
            return(1, lenDiffRatio, inparaVsOrthoLen)
        else: # drop it
            return(0, lenDiffRatio, inparaVsOrthoLen)



def test_extract_paralogs_single_core(debug=True):
    '''Test extraction of paralogs.'''
    root = '/home/salvocos/tmp/test_length_difference_filter/'
    inTblDir = '%sinput/ortholog_tables/'%root
    testTbl = os.path.join(inTblDir, 'table.macaca_mulatta-pan_troglodytes')

    # read the cluster and extract paralogs only from the left part
    for ln in open(testTbl):
        if ln[0] == 'O':
            continue
        ln = ln.rstrip('\n')
        clstrId, clstrSc, subClstrA, subClstrB = ln.rsplit('\t')
        orthoList, inparaDict = extract_paralogs_single_core(subClstrA, debug)
        print(orthoList)
        print(inparaDict)
        if clstrId == '9247':
            print(clstrId)
            break



def test_length_difference_check(debug=True):
    """Test length difference check."""
    # ortholog length
    orthoLen = 100
    # test for differente values
    inparaLengths = [10, 25, 50, 75, 100, 120, 150, 200, 500, 1000]
    # test at difference thresholds
    #thrValues = [0., 0.05, 0.1, 0.15, 0.2, 0.25, 0.3, 0.35, 0.4, 0.45, 0.5, 0.6, 0.7, 0.75, 0.8, 0.9, 1.]
    thrValues = [0., 0.1, 0.2, 0.25, 0.3, 0.4, 0.5, 0.6, 0.7, 0.75, 0.8, 0.9, 1.]

    # start the testing
    for l in inparaLengths:
        # test using different thresholds
        for lenThr in thrValues:
            if debug:
                print('\nChecking with threshold:\t%s'%lenThr)
            print(length_difference_check(orthoLen, l, lenThr, debug))
