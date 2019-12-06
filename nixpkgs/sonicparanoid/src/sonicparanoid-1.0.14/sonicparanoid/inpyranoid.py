'''
This module is used to perform orthology prediction from
alignment files formatted the way inparanoid does.
'''
import os
import sys
import copy
from math import pow
from timeit import default_timer as timer
from collections import OrderedDict
from collections import Counter
from operator import itemgetter
#### IMPORT TO GENERATE PyPi package
#'''
from sonicparanoid import length_difference_filter as lendiffilter
from sonicparanoid import sys_tools as systools
# import Cython module for orthology inference
from sonicparanoid import inpyranoid_c
#'''
####

#### IMPORTS TO RUN LOCALLY
'''
import length_difference_filter as lendiffilter
import sys_tools as systools
#import Cython module for orthology inference
import inpyranoid_c
#'''
####

from numpy import array
import pandas as pd
import multiprocessing as mp
import queue

''' # not required anymore
import importlib
try:
    importlib.import_module("sonicparanoid.inpyranoid_c")
except ImportError as err:
    sys.stderr.write('%s\n'%err)
    sys.stderr.write('\nTry to run the SonicParanoid setup by running the script setup_sonicparanoid.py.\n')
    sys.exit(-5)
else:
    from sonicparanoid import inpyranoid_c
    print('Module inpyranoid_c succesfully loaded!')
'''


__module_name__ = "InPyranoid"
__source__ = "inpyranoid.py"
__author__ = "Salvatore Cosentino"
#__copyright__ = ""
__license__ = "GPL"
__version__ = "1.5"
__maintainer__ = "Cosentino Salvatore"
__email__ = "salvo981@gmail.com"


##### FUNCTIONS #####
def info():
    """This module is used to execute blast and related tools it also include some functions to format blast output."""
    print('MODULE NAME:\t%s'%__module_name__)
    print('SOURCE FILE NAME:\t%s'%__source__)
    print('MODULE VERSION:\t%s'%__version__)
    print('LICENSE:\t%s'%__license__)
    print('AUTHOR:\t%s'%__author__)
    print('EMAIL:\t%s'%__email__)



def consume_alignment_preproc(jobs_queue, results_queue, sharedWithinDict, covCoff=0.25, overlapCoff=0.5, scoreCoff=40):
    """Preprocess a single alignment preproc step"""
    while True:
        current_pair = jobs_queue.get(block=True, timeout=None)
        if current_pair is None:
            break
        # set the length dictionary to a new one for now
        lenDictA = {}
        # preprocess the alignment file
        preprocDict, lenDictA = inpyranoid_c.preprocess_within_align(current_pair, lenDictA, covCoff=covCoff, overlapCoff=overlapCoff, scoreCoff=scoreCoff, debug=False)
        # add the preprocessed information in the shared dictionary
        sp = os.path.basename(current_pair).split('-', 1)[0]
        if not sp in sharedWithinDict:
            sys.exit('ERROR: the species must be in the shared dictionary')
        # return the dictionaries
        results_queue.put((sp, preprocDict, lenDictA))



def check_inpyranoid_module_build():
    import importlib
    try:
        importlib.import_module('inpyranoid_c')
    except ImportError as err:
        print(err)
    else:
        print('Module inpyranoid_c succesfully loaded!')



def cluster_orthologs(ortoCandAB, ortoA, hitsAinA, scoresAA, lenDictA, bestscoreAB, ortoB, hitsBinB, scoresBB, lenDictB, bestscoreBA, confCutoff=0.05, lenDiffThr=0.5, debug=False):
    """Find paralogs and create final clusters."""
    if debug:
        print('\ncluster_orthologs :: START')
        print('Candidate ortholog pairs:\t%d'%len(ortoCandAB))
        print('Hits of A in A:\t%d'%len(hitsAinA))
        print('Scores for AA pairs:\t%d'%len(scoresAA))
        print('Sequence lengths for A:\t%d'%len(lenDictA))
        print('Best scores for AB pairs:\t%d'%len(bestscoreAB))
        print('Hits of B in B:\t%d'%len(hitsBinB))
        print('Scores for BB pairs:\t%d'%len(scoresBB))
        print('Sequence lengths for B:\t%d'%len(lenDictB))
        print('Best scores for BA pairs:\t%d'%len(bestscoreBA))
        print('Paralog confidence cutoff:\t%s'%str(confCutoff))
        print('Length difference filtering threshold:\t%s'%str(lenDiffThr))

    #define dictionaries which will contain candidate paralogs
    membersA = OrderedDict()
    membersB = OrderedDict()
    # these are genes that are found with confidence 1.0 (the core orthologs)
    newOrtoA = {}
    newOrtoB = {}

    earlyClstrDict = {} # will contain a dictionary for A and B elements of the clusters
    # For example, for the pair oA1-oB1 it will contain a dictionary with orthologs and paralogs for both A and B
    # the dictionaries have scores as values so that they can be easily sorted in the final clustering steps
    #start reading the candindates
    for oPair in ortoCandAB:
        oA, oB = oPair.split('!', 1)
        # extract score
        oScore = ortoCandAB[oPair]
        #get best score for the ortholog pair
        bestScPair = bestscoreAB[oPair]
        # check first the withinhits are available
        withinHitsAADict = withinHitsBBDict = None
        if oA in hitsAinA:
            withinHitsAADict = hitsAinA[oA]
        if oB in hitsBinB:
            withinHitsBBDict = hitsBinB[oB]
        # add the ortholog pair to the clusters
        if not oPair in earlyClstrDict:
            earlyClstrDict[oPair] = {}
            earlyClstrDict[oPair][oA] = OrderedDict()
            earlyClstrDict[oPair][oB] = OrderedDict()
        else:
            sys.exit('Ortholog pair %s was already in the cluster list'%oPair)
        # search for orthologs only if the the within hits for AA and BB are available
        if (withinHitsAADict is not None) and (withinHitsBBDict is not None):
            # search paralogs for A
            mA, rjctA = find_paralogs(oA, withinHitsAADict, oScore, bestScPair, scoresAA, newOrtoA, ortoA, earlyClstrDict, oPair, lenDictX=lenDictA, confCutoff=confCutoff, lenDiffThr=lenDiffThr, debug=debug)
            #sys.exit('DEBUG :: cluster_orthologs')
            # search paralogs for B
            mB, rjctB = find_paralogs(oB, withinHitsBBDict, oScore, bestScPair, scoresBB, newOrtoB, ortoB, earlyClstrDict, oPair, lenDictX=lenDictB, confCutoff=confCutoff, lenDiffThr=lenDiffThr, debug=debug)
        else: # otherwise only the add the ortholog pair with confidence 1.0
            #print('This is the simple case!')
            #sys.exit('DEBUG :: cluster_orthologs')
            earlyClstrDict[oPair][oA] = OrderedDict([ (oA, 1.) ])
            earlyClstrDict[oPair][oB] = OrderedDict([ (oB, 1.) ])
    return (earlyClstrDict, newOrtoA, newOrtoB)



def cluster_orthologs_write_rejected(rejectedFilePath, spPair, ortoCandAB, ortoA, hitsAinA, scoresAA, lenDictA, bestscoreAB, ortoB, hitsBinB, scoresBB, lenDictB, bestscoreBA, confCutoff=0.05, lenDiffThr=0.5, debug=False):
    """Find paralogs and create final clusters."""
    if debug:
        print('\ncluster_orthologs_write_rejected :: START')
        print('File in which the rejected ortholog genes will be written:\t%s'%str(rejectedFilePath))
        print('Species pair (e.g., A_B):\t{:s}'.format(spPair))
        print('Candidate ortholog pairs:\t%d'%len(ortoCandAB))
        print('Hits of A in A:\t%d'%len(hitsAinA))
        print('Scores for AA pairs:\t%d'%len(scoresAA))
        print('Sequence lengths for A:\t%d'%len(lenDictA))
        print('Best scores for AB pairs:\t%d'%len(bestscoreAB))
        print('Hits of B in B:\t%d'%len(hitsBinB))
        print('Scores for BB pairs:\t%d'%len(scoresBB))
        print('Sequence lengths for B:\t%d'%len(lenDictB))
        print('Best scores for BA pairs:\t%d'%len(bestscoreBA))
        print('Paralog confidence cutoff:\t%s'%str(confCutoff))
        print('Length difference filtering threshold:\t%s'%str(lenDiffThr))

    #define dictionaries which will contain candidate paralogs
    membersA = OrderedDict()
    membersB = OrderedDict()
    # these are genes that are found with confidence 1.0 (the core orthologs)
    newOrtoA = {}
    newOrtoB = {}

    # NOTE: only for testing (to be removed)
    rjctA = {}
    rjctB = {}
    #########

    # open the file with rejected orthologs
    rjFd = open(rejectedFilePath, 'w')

    earlyClstrDict = {} # will contain a dictionary for A and B elements of the clusters
    # For example, for the pair oA1-oB1 it will contain a dictionary with orthologs and paralogs for both A and B
    # the dictionaries have scores as values so that they can be easily sorted in the final clustering steps
    #start reading the candindates
    for oPair in ortoCandAB:
        oA, oB = oPair.split('!', 1)
        # extract score
        oScore = ortoCandAB[oPair]
        #get best score for the ortholog pair
        bestScPair = bestscoreAB[oPair]
        # check first the withinhits are available
        withinHitsAADict = withinHitsBBDict = None
        if oA in hitsAinA:
            withinHitsAADict = hitsAinA[oA]
        if oB in hitsBinB:
            withinHitsBBDict = hitsBinB[oB]
        # add the ortholog pair to the clusters
        if not oPair in earlyClstrDict:
            earlyClstrDict[oPair] = {}
            earlyClstrDict[oPair][oA] = OrderedDict()
            earlyClstrDict[oPair][oB] = OrderedDict()
        else:
            sys.exit('Ortholog pair %s was already in the cluster list'%oPair)
        # search for orthologs only if the the within hits for AA and BB are available
        if (withinHitsAADict is not None) and (withinHitsBBDict is not None):
            # search paralogs for A
            mA, rjctA = find_paralogs(oA, withinHitsAADict, oScore, bestScPair, scoresAA, newOrtoA, ortoA, earlyClstrDict, oPair, lenDictX=lenDictA, confCutoff=confCutoff, lenDiffThr=lenDiffThr, debug=debug)
            if len(rjctA) > 0:
                #print('\n@cluster_orthologs_write_rejected :: rejected for A:\t%d'%len(rjctA))
                for rjGene in rjctA:
                    #print('%s\t%s'%(rjGene, rjctA[rjGene]))
                    inpSc, multiplier, rjV1, rjV2 = rjctA[rjGene]
                    #rjFd.write('{:s}_{:s}\t{:s}\t{:s}\n'.format(spPair, rjGene, str(rjV1), str(rjV2)))
                    #rjFd.write('{:s}\t{:s}\t{:s}\n'.format(rjGene, str(rjV1), str(rjV2)))
                    rjFd.write('{:s}\t{:s}\t{:s}\t{:s}\t{:s}\n'.format(rjGene, str(inpSc), str(multiplier), str(rjV1), str(rjV2)))
                #print()
            #sys.exit('DEBUG :: cluster_orthologs_write_rejected')
            # search paralogs for B
            mB, rjctB = find_paralogs(oB, withinHitsBBDict, oScore, bestScPair, scoresBB, newOrtoB, ortoB, earlyClstrDict, oPair, lenDictX=lenDictB, confCutoff=confCutoff, lenDiffThr=lenDiffThr, debug=debug)
            if len(rjctB) > 0:
                #print('\n@cluster_orthologs_write_rejected :: rejected for B:\t%d'%len(rjctB))
                for rjGene in rjctB:
                    #print('%s\t%s'%(rjGene, rjctB[rjGene]))
                    inpSc, multiplier, rjV1, rjV2 = rjctB[rjGene]
                    #rjFd.write('{:s}_{:s}\t{:s}\t{:s}\n'.format(spPair, rjGene, str(rjV1), str(rjV2)))
                    #rjFd.write('{:s}\t{:s}\t{:s}\n'.format(rjGene, str(rjV1), str(rjV2)))
                    rjFd.write('{:s}\t{:s}\t{:s}\t{:s}\t{:s}\n'.format(rjGene, str(inpSc), str(multiplier), str(rjV1), str(rjV2)))
                #print()
        else: # otherwise only the add the ortholog pair with confidence 1.0
            #print('This is the simple case!')
            #sys.exit('DEBUG :: cluster_orthologs_write_rejected')
            earlyClstrDict[oPair][oA] = OrderedDict([ (oA, 1.) ])
            earlyClstrDict[oPair][oB] = OrderedDict([ (oB, 1.) ])
    # NOTE: original
    #return (earlyClstrDict, newOrtoA, newOrtoB)
    # NOTE: only for testing (to be removed)
    #print(len(rjctA), len(rjctB))
    rjFd.close()
    #sys.exit('DEBUG :: cluster_orthologs_write_rejected')
    return (earlyClstrDict, newOrtoA, newOrtoB)



def equalize_AB_and_BA_scores(abScores, baScores, debug=False):
    """Equalize the scores from AB and BA alignments."""
    # for example given the hit g-c in AB with score x,
    # and given the hits c-g in BA with score y
    # a uniq values for BA[c-g] and AB[g-c] are calculated and the corresponding
    # dictionaries updated accordingly
    if debug:
        print('equalize_AB_and_BA_scores :: START')
        print('Hits for AB: %d'%len(abScores))
        print('Hits for BA: %d'%len(baScores))
    # check input lengths
    if len(abScores) != len(baScores):
        sys.write.stderr('ERROR: the two dictionaries must contain the same number of hits')
        sys.exit(-5)

    #start looping the hits in AB
    for k in abScores:
        scAB = abScores[k]
        #get revese key
        q, s = k.split('!', 1)
        k2 = '%s!%s'%(s, q)
        scBA = baScores[k2]
        #calculate the average value
        #avgScore = int(float((scAB + scBA) / 2.) + .5) # NOTE: this is not correct!
        #correct rounding
        avgScore = int(round(float((scAB + scBA) / 2.), 0))
        abScores[k] = baScores[k2] = avgScore
    return (abScores, baScores)



def find_paralogs(qX, withinHitsDict, oScore, bestscoreXY, scoresXX, newOrtoX, ortoX, clstrsDict, clstrKey, lenDictX, confCutoff=0.05, lenDiffThr=0.5, debug=False):
    """Find paralogs in either A or B, for the input ortholog candidate and corresponding homologs (hits).

    This version also filters out potential orthologs based on the length difference with the inparalogs.
    """
    if debug:
        print('\nfind_paralogs :: START')
        print('Ortholog gene in species X:\t%s'%qX)
        print('Related within hits (%d):\t%s'%(len(withinHitsDict), withinHitsDict))
        print('Orholog score for %s:\t%s'%(qX, str(oScore)))
        print('Best for the current ortholog pairs:\t%s'%str(bestscoreXY))
        print('Within-scores for proteome X:\t%d'%len(scoresXX))
        print('Orthologs for species X:\t%d'%len(ortoX))
        print('Clusters (orthologs and paralogs) dictionary:\t%d'%len(clstrsDict))
        print('Orthologs pair used as keys:\t%s'%(clstrKey))
        print('Sequence lengths for proteome X:\t%d'%len(lenDictX))
        print('Confidence cutoff for paralogs:\t%s'%str(confCutoff))
        print('Length difference filtering threshold:\t%s'%str(lenDiffThr))

    oScore = float(oScore)
    membersX = OrderedDict() # will contain ortholog and paralogs
    tmpWithin_qq = '%s!%s'%(qX, qX)
    # Score associated to the ortholog qX when aligned to itself in the within-alignment XX
    scXXqq = 0.

    # contains the gene of each rejected inpralogs based on length
    # and tuple containing the length difference ratio how many times
    # it differes with the orthologs
    rjctDict = OrderedDict()

    if tmpWithin_qq in scoresXX:
        scXXqq = float(scoresXX[tmpWithin_qq])
    else:
        # this happens when the best within-hit is not with the query itself
        # for example, g1 has best match with g2 (instead of g1) in the same species
        if debug:
            #sys.stderr.write('WARNING: best within hit for {:s} was not with the query itself'.format(qX))
            print('WARNING: no best within-proteome score found for the paralogs for query {:s} and for ortholog pair {:s}'.format(qX, clstrKey))
            print('The corresponding within-score will be set to 0 and the paralog skipped.')
        # NOTE: this code could be used later to avoid adding
        #membersX[qX] = 1.0
        #newOrtoX[qX] = None
        #clstrsDict[clstrKey][qX] = membersX
        #return (membersX, rjctDict)

    # get the length of the ortholog
    qXLen = lenDictX[qX]

    #search for candidate paralogs for each hit
    for hX in withinHitsDict:
        tmpWithin_qh = '%s!%s'%(qX, hX)
        #print()
        #print(tmpWithin_qh)
        scXXqh = float(scoresXX[tmpWithin_qh])
        #set the conditions for a hit to be considered paralog
        # original perl code:
        #if ( ($idA == $hitID) or ($scoreAA{"$idA:$hitID"} >= $bestscoreAB[$idA]) and ($scoreAA{"$idA:$hitID"} >= $bestscoreAB[$hitID]))
        #NOTE: the AND condition is always true since $bestscoreAB[$idA] and $bestscoreAB[$hitID] must be same since they been previously equalized
        confSc = 0.
        ##### VERSION SLIGHTLY MODIFIED from the original SONICPARANOID #####
        #### If the orhtolog is compared to itself we just give score 1.0 ####
        #'''
        if (qX == hX):
            confSc = 1.0
        elif (scXXqh >= bestscoreXY):
            # then we have found a candidate paralog
            # set the confidence score for the paralog
            # if the score for the paralog is higher than that of the ortholog
            # then it could a secondary ortholog
            #sys.exit('DEBUG :: find_paralogs')
            # get length of the potential inparalog
            hXLen = lenDictX[hX]
            # check if the inparalog should be kept or removed (give confidence score < 0)
            keep, lenDiffRatio, inpaVsOrtoLen = lendiffilter.length_difference_check(qXLen, hXLen, lenDiffThr, debug=debug)
            if not keep: # give confSc of 0 and continue
                ##rjctDict[hX] = (lenDiffRatio, inpaVsOrtoLen)
                confSc = 0 # avoid the score calculation if rejcted
            elif scXXqq == oScore:
                if scXXqh == scXXqq:
                    confSc = 1.
            else: # it is just a paralog hence we calculate the score
                '''
                #### ORGINAL ####
                confSc = round(float(scXXqh - oScore) / float(scXXqq - oScore), 3)
                if confSc > 1:
                    #print(confSc)
                    confSc = 1.
                ################
                #'''
                #'''
                #### DIVIDE BY DIFFLEN ####
                #if (lenDiffRatio == 0.) or (lenDiffRatio == 1.):
                if lenDiffRatio == 0:
                    confSc = round(float(scXXqh - oScore) / float(scXXqq - oScore), 3)
                else:
                    ''' Version originally submitted (wrong behavior!)
                    qhLenScore = float(scXXqh - oScore)/float(lenDiffRatio)
                    qqLenScore = float(scXXqq - oScore)/float(1. - lenDiffRatio)
                    confSc = round(qhLenScore / float(qqLenScore), 3)
                    #'''

                    #''' Revised formula that avoid the erouned behaviour of
                    # rewarding pairs with high length difference
                    # and penalizing pairs with similar lengths
                    # f(x) = x * (1-ldr)
                    #multiPlier = float(1. - lenDiffRatio)

                    # exponetial functions
                    # f(x) = x * (1 - exp((ldr-1)/ldr)) # question
                    #multiPlier = 1. - exp((lenDiffRatio - 1.) / lenDiffRatio)
                    # f(x) = x * (1 - 2^((ldr-1)/ldr))
                    #multiPlier = 1. - pow(2, (lenDiffRatio - 1.) / lenDiffRatio)
                    # f(x) = x * (1 - (3/2)^((ldr-1)/ldr))
                    multiPlier = 1. - pow(float(3/2), (lenDiffRatio - 1.) / lenDiffRatio)

                    # Inparanoid formula
                    #multiPlier = 1.

                    # calculate the score score
                    # inparaSc = round(float(scXXqh - oScore) / float(scXXqq - oScore), 3) # needed if the we want the rejected orthologs in output
                    confSc = round((float(scXXqh - oScore) / float(scXXqq - oScore)) * multiPlier, 3)

                    # Inparanoid formula
                    #confSc = round(float(scXXqh - oScore) / float(scXXqq - oScore) , 3)
                    #'''

                    #NOTE: to be removed
                    '''
                    ##### ONLY FOR TESTING ####
                    if (inpaVsOrtoLen > 1) or (inpaVsOrtoLen < -1):
                        if confSc >= confCutoff:
                            if confSc < 1:
                                rjctDict[hX] = (inparaSc, multiPlier, lenDiffRatio, inpaVsOrtoLen)
                    ###########################
                    '''

                if confSc > 1:
                    #print(confSc)
                    confSc = 1.
                ################
                #'''

                if debug:
                    print('conf case 2')
                    print(scXXqh)
                    print(scXXqq)
                    print(oScore)

        # consider if the gene should be added to the cluster
        #oCnt = pCnt = 0

        ##### VERSION IN THE WORKING VERSION OF SONICPARANOID ######
        #'''
        if confSc >= confCutoff:
            membersX[hX] = confSc
            if confSc == 1.0:
                newOrtoX[hX] = None
        #'''
        ############################################################

    # add the members to the clusters using the corresponding key
    if scXXqq == 0: #only add the ortholog gene with confidence 1.0
        membersX[hX] = 1.0
        newOrtoX[hX] = None
    # add orthologs to groups
    clstrsDict[clstrKey][qX] = membersX
    return (membersX, rjctDict)



def infer_orthologs(pathA, pathB, alignDir=os.getcwd(), outDir=os.getcwd(), confCutoff=0.05, lenDiffThr=0.5, debug=False):
    """Infer orthology for the two input proteomes."""
    if debug:
        print('\ninfer_orthologs :: START')
        print('Input proteome 1:%s'%pathA)
        print('Input proteome 2:%s'%pathB)
        print('Outdir:%s'%outDir)
        print('Alignments dir:%s'%alignDir)
        print('Confidence cutoff for paralogs:\t%s'%str(confCutoff))
        print('Length difference filtering threshold:\t%s'%str(lenDiffThr))
    #sys.exit('DEBUG :: infer_orthologs :: START')
    # start timer
    start_time = timer()
    #check the existence of the input file
    if not os.path.isfile(pathA):
        sys.stderr.write('ERROR: The first input file %s was not found, please provide the path to a valid file.\n'%pathA)
        sys.exit(-2)
    if not os.path.isfile(pathB):
        sys.stderr.write('ERROR: The second input file %s was not found, please provide the path to a valid file.\n'%pathB)
        sys.exit(-2)
    #create the output file name
    species1 = os.path.basename(pathA)
    species2 = os.path.basename(pathB)
    # check that the alignment files exist
    #pathAA = os.path.join(outDir, '{:s}-{:s}'.format(species1, species1))
    pathAA = os.path.join(alignDir, '{:s}-{:s}'.format(species1, species1))
    if not os.path.isfile(pathAA):
        sys.stderr.write('ERROR: the alignment file (%s) was not found, it is required to perform orthology inference.\n'%pathAA)
        sys.exit(-2)
    ##pathBB = os.path.join(outDir, '{:s}-{:s}'.format(species2, species2))
    pathBB = os.path.join(alignDir, '{:s}-{:s}'.format(species2, species2))
    if not os.path.isfile(pathBB):
        sys.stderr.write('ERROR: the alignment file (%s) was not found, it is required to perform orthology inference.\n'%pathBB)
        sys.exit(-2)
     #pathAB = os.path.join(outDir, '{:s}-{:s}'.format(species1, species2))
    pathAB = os.path.join(alignDir, '{:s}-{:s}'.format(species1, species2))
    if not os.path.isfile(pathAB):
        sys.stderr.write('ERROR: the alignment file (%s) was not found, it is required to perform orthology inference.\n'%pathAB)
        sys.exit(-2)
    #pathBA = os.path.join(outDir, '{:s}-{:s}'.format(species2, species1))
    pathBA = os.path.join(alignDir, '{:s}-{:s}'.format(species2, species1))
    if not os.path.isfile(pathBA):
        sys.stderr.write('ERROR: the alignment file (%s) was not found, it is required to perform orthology inference.\n'%pathBA)
        sys.exit(-2)
    if debug:
        print('AB:\t%s'%pathAB)
        print('BA:\t%s'%pathBA)
        print('AA:\t%s'%pathAA)
        print('BB:\t%s\n'%pathBB)
    #sys.exit('DEBUG :: infer_orthologs :: after alignments file check')
    # Match area should cover at least this much of longer sequence.
    # Match area is defined as length from the start of first segment to end of last segment
    # i.e segments 1-10, 20-25, and 80-90 gives a match length of 90.
    segOverlapCutoff = 0.5
    # The actual matching segments must cover this of this match of the matched sequence
	# For example for a matched sequence 70 bps long, segments 1-15 and 50-70 gives a total coverage of 35, which is 50% of total.
    segCoverageCutoff = 0.25
    #score cutoff for alignments
    scoreCutoff = 40

    ####### LOAD BETWEEN PROTEOMES ALIGNMENTS ##########
    #start timer
    load_between_proteomes_scores_start = timer()
    # Cython version
    scoresAB, hitsAinB, scoresBA, hitsBinA, lenDictA, lenDictB = inpyranoid_c.load_between_proteomes_scores_fast(pathAB, pathBA, covCoff=segCoverageCutoff, overlapCoff=segOverlapCutoff, scoreCoff=scoreCutoff, debug=debug)
    load_between_proteomes_scores_end = timer()
    if debug:
        print('\nload_between_proteomes_scores exec time:\t%s\n'%(str(round(load_between_proteomes_scores_end - load_between_proteomes_scores_start, 3))))
    ##################################################
    #sys.exit('DEBUG :: infer_orthologs :: after loading between proteome scores.')
    #equalize between proteome scores
    scoresAB, scoresBA = equalize_AB_and_BA_scores(scoresAB, scoresBA, debug=debug)
    equalize_timer_end = timer()
    if debug:
        print('\nequalize_AB_and_BA_scores exec time:\t%s\n'%(str(round(equalize_timer_end - load_between_proteomes_scores_end, 3))))
    #load best hits for AB and BA
    bestHitsAB, bestHitsBA, bestscoreAB, bestscoreBA = load_besthits_between_proteomes(hitsAinB, hitsBinA, scoresAB, scoresBA, debug=debug)
    load_besthits_timer_end = timer()
    if debug:
        print('\nload_besthits_between_proteomes exec time:\t%s\n'%(str(round(load_besthits_timer_end - equalize_timer_end, 3))))
    #sys.exit('DEBUG :: infer_orthologs :: after load_besthits_between_proteomes.')
    #find candidate orthologs
    ortoA, ortoB, ortoCandAB = find_orthologs_between_proteomes_bestscores(scoresAB, scoresBA, bestscoreAB, bestscoreBA, debug=debug)
    find_orthologs_timer_end = timer()
    if debug:
        print('\nfind_orthologs_between_proteomes_bestscores exec time:\t%s\n'%(str(round(find_orthologs_timer_end - load_besthits_timer_end, 3))))
    #sys.exit('DEBUG :: infer_orthologs :: after find_orthologs_between_proteomes_bestscores.')

    ####### LOAD WITHIN PROTEOMES ALIGNMENTS ##########
    # Cython version
    #scoresAA, hitsAinA, scoresBB, hitsBinB, lenDictA, lenDictB = inpyranoid_c.load_within_proteomes_scores_fast(pathAA, pathBB, ortoA, ortoB, lenDictA, lenDictB, covCoff=segCoverageCutoff, overlapCoff=segOverlapCutoff, scoreCoff=scoreCutoff, debug=False)

    # load the within alignments in 2 steps
    # for AA
    scoresAA, hitsAinA, lenDictA = inpyranoid_c.load_within_proteomes_scores_single(pathAA, ortoA, lenDictA, covCoff=segCoverageCutoff, overlapCoff=segOverlapCutoff, scoreCoff=scoreCutoff, debug=False)

    # for BB
    scoresBB, hitsBinB, lenDictB = inpyranoid_c.load_within_proteomes_scores_single(pathBB, ortoB, lenDictB, covCoff=segCoverageCutoff, overlapCoff=segOverlapCutoff, scoreCoff=scoreCutoff, debug=False)

    load_within_proteomes_scores_end = timer()
    if debug:
        print('\nload within proteomes scores exec time:\t%s\n'%(str(round(load_within_proteomes_scores_end - find_orthologs_timer_end, 3))))
    #####################################################
    #sys.exit('DEBUG :: infer_orthologs :: after load_within_proteomes_scores_fast.')

    #search for paralogs and generate final clusters
    orthoClstrs, coreOrtoA, coreOrtoB = cluster_orthologs(ortoCandAB, ortoA, hitsAinA, scoresAA, lenDictA, bestscoreAB, ortoB, hitsBinB, scoresBB, lenDictB, bestscoreBA, confCutoff=confCutoff, lenDiffThr=lenDiffThr, debug=debug)
    cluster_orthologs_end = timer()
    if debug:
        print('\ncluster_orthologs exec time:\t%s\n'%(str(round(cluster_orthologs_end - load_within_proteomes_scores_end, 3))))
    #sys.exit('DEBUG :: infer_orthologs :: after cluster_orthologs.')
    # output prefix
    # USE THIS IF NO MERGING IS PERFORMED
    # outName = '{:s}-{:s}'format(species1, species2)
    outName = '{:s}-{:s}.umerged'.format(species1, species2)
    # write output files
    #write_inpyranoid_output(orthoClstrs, ortoCandAB, coreOrtoA, coreOrtoB, outName, outDir=outDir, writeTbl=True, writeRelations=False, debug=debug)
    tblOutPath = write_inpyranoid_output_simple(orthoClstrs, ortoCandAB, coreOrtoA, coreOrtoB, outName, outDir=outDir, debug=debug)
    merge_and_write_inpyranoid_output(tblOutPath, debug=debug)
    #sys.exit('DEBUG :: infer_orthologs :: after write_inpyranoid_output.')
    write_output_end = timer()
    if debug:
        print('\nwrite_inpyranoid_output exec time:\t%s\n'%(str(round(write_output_end - cluster_orthologs_end, 3))))
        print('\ntotal execution time:\t%s\n'%(str(round(write_output_end - start_time, 3))))
    #sys.exit('DEBUG :: infer_orthologs :: after write_inpyranoid_output.')



def infer_orthologs_shared_dict(pathA, pathB, alignDir=os.getcwd(), outDir=os.getcwd(), sharedWithinDict=None, confCutoff=0.05, lenDiffThr=0.5, debug=False):
    """
    Infer orthology for the two input proteomes.
    Shared dictionaries are used to save processing time.
    """
    if debug:
        print('\ninfer_orthologs_shared_dict :: START')
        print('Input proteome 1:%s'%pathA)
        print('Input proteome 2:%s'%pathB)
        print('Outdir:%s'%outDir)
        print('Alignments dir:%s'%alignDir)
        print('Species with shared info:\t%d'%(len(sharedWithinDict)))
        print('Confidence cutoff for paralogs:\t%s'%str(confCutoff))
        print('Length difference filtering threshold:\t%s'%str(lenDiffThr))
    #sys.exit('DEBUG :: ninfer_orthologs_shared_dict :: START')
    # start timer
    start_time = timer()
    #check the existence of the input file
    if not os.path.isfile(pathA):
        sys.stderr.write('ERROR: The first input file %s was not found, please provide the path to a valid file.\n'%pathA)
        sys.exit(-2)
    if not os.path.isfile(pathB):
        sys.stderr.write('ERROR: The second input file %s was not found, please provide the path to a valid file.\n'%pathB)
        sys.exit(-2)
    #create the output file name
    species1 = os.path.basename(pathA)
    species2 = os.path.basename(pathB)
    # create path names
    pathAB = os.path.join(alignDir, '{:s}-{:s}'.format(species1, species2))
    if not os.path.isfile(pathAB):
        sys.stderr.write('ERROR: the alignment file (%s) was not found, it is required to perform orthology inference.\n'%pathAB)
        sys.exit(-2)
    pathBA = os.path.join(alignDir, '{:s}-{:s}'.format(species2, species1))
    if not os.path.isfile(pathBA):
        sys.stderr.write('ERROR: the alignment file (%s) was not found, it is required to perform orthology inference.\n'%pathBA)
        sys.exit(-2)
    if debug:
        print('AB:\t%s'%pathAB)
        print('BA:\t%s'%pathBA)
        #print('AA:\t%s'%pathAA)
        #print('BB:\t%s\n'%pathBB)
    #sys.exit('DEBUG :: ninfer_orthologs_shared_dict :: after alignments file check')
    # Match area should cover at least this much of longer sequence.
    # Match area is defined as length from the start of first segment to end of last segment
    # i.e segments 1-10, 20-25, and 80-90 gives a match length of 90.

    #### ORIGINAL ####
    # segOverlapCutoff = 0.5
    ##################
    segOverlapCutoff = 0.20

    # The actual matching segments must cover this of this match of the matched sequence
	# For example for a matched sequence 70 bps long, segments 1-15 and 50-70 gives a total coverage of 35, which is 50% of total.
    segCoverageCutoff = 0.20
    #score cutoff for alignments
    scoreCutoff = 40

    ####### LOAD BETWEEN PROTEOMES ALIGNMENTS ##########
    #start timer
    load_between_proteomes_scores_start = timer()
    # Cython version
    scoresAB, hitsAinB, scoresBA, hitsBinA, lenDictAbetween, lenDictBbetween = inpyranoid_c.load_between_proteomes_scores_fast(pathAB, pathBA, covCoff=segCoverageCutoff, overlapCoff=segOverlapCutoff, scoreCoff=scoreCutoff, debug=debug)
    load_between_proteomes_scores_end = timer()
    if debug:
        print('\nload_between_proteomes_scores exec time:\t%s\n'%(str(round(load_between_proteomes_scores_end - load_between_proteomes_scores_start, 3))))
    ##################################################
    #sys.exit('DEBUG :: infer_orthologs_shared_dict :: after loading between proteome scores.')
    #equalize between proteome scores
    #scoresAB, scoresBA = equalize_AB_and_BA_scores(scoresAB, scoresBA, debug=debug)
    #equalize fast mode
    scoresAB, scoresBA = inpyranoid_c.equalize_AB_and_BA_scores_fast(scoresAB, scoresBA, debug=debug)
    #print(len(scoresAB), len(scoresBA))
    equalize_timer_end = timer()
    if debug:
        print('\nequalize_AB_and_BA_scores exec time:\t%s\n'%(str(round(equalize_timer_end - load_between_proteomes_scores_end, 3))))
    #load best hits for AB and BA
    bestHitsAB, bestHitsBA, bestscoreAB, bestscoreBA = load_besthits_between_proteomes(hitsAinB, hitsBinA, scoresAB, scoresBA, debug=debug)
    load_besthits_timer_end = timer()
    if debug:
        print('\nload_besthits_between_proteomes exec time:\t%s\n'%(str(round(load_besthits_timer_end - equalize_timer_end, 3))))
    #sys.exit('DEBUG :: infer_orthologs_shared_dict :: after load_besthits_between_proteomes.')
    #find candidate orthologs
    ortoA, ortoB, ortoCandAB = find_orthologs_between_proteomes_bestscores(scoresAB, scoresBA, bestscoreAB, bestscoreBA, debug=debug)
    find_orthologs_timer_end = timer()
    if debug:
        print('\nfind_orthologs_between_proteomes_bestscores exec time:\t%s\n'%(str(round(find_orthologs_timer_end - load_besthits_timer_end, 3))))
    #sys.exit('DEBUG :: infer_orthologs_shared_dict :: after find_orthologs_between_proteomes_bestscores.')

    ####### LOAD WITHIN PROTEOMES ALIGNMENTS ##########
    # process AA within alignments
    preprocAADict = sharedWithinDict[species1][1]
    lenDictA = sharedWithinDict[species1][2]
    scoresAA, hitsAinA, lenDictA = postprocess_within_align(preprocAADict, ortoA, lenDictA, lenDictAbetween, debug=False)
    #sys.exit('DEBUG :: infer_orthologs_shared_dict :: after postprocess_within_align AA ({:s})'.format(species1))

    # process BB within alignments
    preprocBBDict = sharedWithinDict[species2][1]
    lenDictB = sharedWithinDict[species2][2]
    scoresBB, hitsBinB, lenDictB = postprocess_within_align(preprocBBDict, ortoB, lenDictB, lenDictBbetween, debug=False)
    #sys.exit('DEBUG :: infer_orthologs_shared_dict :: after postprocess_within_align BB ({:s})'.format(species2))

    load_within_proteomes_scores_end = timer()
    if debug:
        print('\nload within proteomes scores exec time:\t%s\n'%(str(round(load_within_proteomes_scores_end - find_orthologs_timer_end, 3))))
    #####################################################
    #sys.exit('DEBUG :: infer_orthologs_shared_dict :: after load_within_proteomes_scores_fast.')

    #NOTE: this only for testing
    '''
    #### write rejected inparalogs ####
    # set path to the file with rejected inparalogs
    rejctFileName = '{:s}-{:s}.difflen.tsv'.format(species1, species2)
    rejctFilePath = os.path.join(outDir, rejctFileName)
    #sys.exit('DEBUG :: inpyranoid :: infer_orthologs_shared_dict')

    orthoClstrs, coreOrtoA, coreOrtoB = cluster_orthologs_write_rejected(rejctFilePath, '{:s}_{:s}'.format(species1, species2), ortoCandAB, ortoA, hitsAinA, scoresAA, lenDictA, bestscoreAB, ortoB, hitsBinB, scoresBB, lenDictB, bestscoreBA, confCutoff=confCutoff, lenDiffThr=lenDiffThr, debug=debug)
    '''
    ##################


    #### ORIGINAL ####
    #search for paralogs and generate final clusters
    orthoClstrs, coreOrtoA, coreOrtoB = cluster_orthologs(ortoCandAB, ortoA, hitsAinA, scoresAA, lenDictA, bestscoreAB, ortoB, hitsBinB, scoresBB, lenDictB, bestscoreBA, confCutoff=confCutoff, lenDiffThr=lenDiffThr, debug=debug)
    cluster_orthologs_end = timer()
    if debug:
        print('\ncluster_orthologs exec time:\t%s\n'%(str(round(cluster_orthologs_end - load_within_proteomes_scores_end, 3))))
    #sys.exit('DEBUG :: infer_orthologs_shared_dict :: after cluster_orthologs.')
    # output prefix
    # USE THIS IF NO MERGING IS PERFORMED
    # outName = '{:s}-{:s}'format(species1, species2)
    outName = '{:s}-{:s}.umerged'.format(species1, species2)
    # write output files
    #write_inpyranoid_output(orthoClstrs, ortoCandAB, coreOrtoA, coreOrtoB, outName, outDir=outDir, writeTbl=True, writeRelations=False, debug=debug)
    tblOutPath = write_inpyranoid_output_simple(orthoClstrs, ortoCandAB, coreOrtoA, coreOrtoB, outName, outDir=outDir, debug=debug)

    # remove not required data structures
    del orthoClstrs, ortoCandAB, coreOrtoA, coreOrtoB
    del scoresAA, hitsAinA, scoresBB, hitsBinB, scoresAB, scoresBA
    del hitsAinB, hitsBinA, lenDictAbetween, lenDictBbetween

    merge_and_write_inpyranoid_output(tblOutPath, debug=debug)
    #sys.exit('DEBUG :: infer_orthologs_shared_dict :: after write_inpyranoid_output.')
    write_output_end = timer()
    if debug:
        print('\nwrite_inpyranoid_output exec time:\t%s\n'%(str(round(write_output_end - cluster_orthologs_end, 3))))
        print('\ntotal execution time:\t%s\n'%(str(round(write_output_end - start_time, 3))))
    #sys.exit('DEBUG :: inpyranoid :: infer_orthologs_shared_dict :: after write_inpyranoid_output.')



def find_orthologs_between_proteomes_bestscores(scoresAB, scoresBA, bestscoreAB, bestscoreBA, debug=False):
    """Find candidate orthologs and sort them by score and id."""
    if debug:
        print('\nfind_orthologs_between_proteomes_bestscores :: START')
        print('AB scores: %d'%len(scoresAB))
        print('BA scores: %d'%len(scoresBA))
        print('Best scores AB: %d'%len(bestscoreAB))
        print('Best scores BA: %d'%len(bestscoreBA))
    ortoCandAB = OrderedDict() # will contain the candidate orthologs for AB
    ortoA = {} # will contain the candidate orthologs for A
    ortoB = {} # will contain the candidate orthologs for B
    # USE BEST SCORES
    ##### each hit is an ortholog if q and hit are best hits "have a best score" in both AB and BA #####
    for kAB in bestscoreAB:
        #print("QUERY:\t%s"%q)
        scAB = int(bestscoreAB[kAB])
        tmpQ, tmpM = kAB.split('!')
        kBA = '%s!%s'%(tmpM, tmpQ)
        if kBA in bestscoreBA:
            scBA = int(bestscoreBA[kBA])
            if scBA == scAB:
                ortoCandAB[kAB] = scoresAB[kAB]
                ortoA[tmpQ] = None
                ortoB[tmpM] = None
            else:
                print('Scores are different!')
                sys.exit('ERROR!')
    #sort the candidate orthologs by score
    tplList = [(k, ortoCandAB[k]) for k in sorted(ortoCandAB, key=ortoCandAB.get, reverse=True)]
    ortoCandAB = OrderedDict() #reset the dictionary
    for tpl in tplList:
        ortoCandAB[tpl[0]] = tpl[1]
    # sort orthlogs for A by key
    ortoA = OrderedDict(sorted(ortoA.items()))
    ortoB = OrderedDict(sorted(ortoB.items()))
    if debug:
        print('Candidate orthologs for AB:\t%d'%len(ortoCandAB))
        print('Candidate orthologs for A:\t%d'%len(ortoA))
        print('Candidate orthologs for B:\t%d'%len(ortoB))
    return (ortoA, ortoB, ortoCandAB)



def load_between_proteomes_scores(alignFileAB, alignFileBA, covCoff=0.25, overlapCoff=0.5, scoreCoff=40, debug=False):
    """Load between proteomes scores for AB and BA alignments."""
    if debug:
        print('load_between_proteomes_scores :: START')
        print('Input AB: %s'%alignFileAB)
        print('Input BA: %s'%alignFileBA)
        print('Coverage cutoff: %s'%str(covCoff))
        print('Overlap cutoff: %s'%str(overlapCoff))
        print('Overlap cutoff: %s'%str(scoreCoff))
    if not os.path.isfile(alignFileAB):
        sys.stderr.write('The file %s was not found, please provide a valid input path'%alignFileAB)
        sys.exit(-2)
    if not os.path.isfile(alignFileBA):
        sys.stderr.write('The file %s was not found, please provide a valid input path'%alignFileBA)
        sys.exit(-2)
    #create the dictionaries to store the ids and scores
    scoreAB = OrderedDict()
    scoreBA = OrderedDict()
    hitsAinB = OrderedDict()
    hitsBinA = OrderedDict()
    #counters to see what scores are kept
    overlapSkipCntAB = lowScoreCntAB = okCntAB = 0
    #start reading the scores file
    for ln in open(alignFileAB):
        q, s, score, qlen, slen, qAggrMatch, sAggrMatch, qLocMatch, sLocMatch, dummy = ln.split('\t', 9)
        score = float(score)
        qlen = int(qlen)
        slen = int(slen)
        qAggrMatch = int(qAggrMatch)
        sAggrMatch = int(sAggrMatch)
        qLocMatch = int(qLocMatch)
        sLocMatch = int(sLocMatch)
        #check if the alignment should be used
        if score < scoreCoff:
            lowScoreCntAB += 1
            continue
        if not overlap_test(qlen, slen, qAggrMatch, sAggrMatch, qLocMatch, sLocMatch, covCoff, overlapCoff,  debug=debug):
            overlapSkipCntAB += 1
            continue
        okCntAB += 1
        #now same the hit scores
        hitId = '%s!%s'%(q, s)
        hitIdRev = '%s!%s'%(s, q)
        #save the score
        scoreAB[hitId] = score
        #add match and scores for each query sequence
        if not q in hitsAinB:
            hitsAinB[q] = OrderedDict()
        hitsAinB[q][s] = score
        #initialize the mutual score to the score cut-off
        scoreBA[hitIdRev] = scoreCoff

    # NOW LOAD THE SCORES FOR BA
    # counters to see what scores are kept
    overlapSkipCntBA = lowScoreCntBA = okCntBA = 0
    # start reading the scores file
    for ln in open(alignFileBA):
        q, s, score, qlen, slen, qAggrMatch, sAggrMatch, qLocMatch, sLocMatch, dummy = ln.split('\t', 9)
        score = float(score)
        qlen = int(qlen)
        slen = int(slen)
        qAggrMatch = int(qAggrMatch)
        sAggrMatch = int(sAggrMatch)
        qLocMatch = int(qLocMatch)
        sLocMatch = int(sLocMatch)
        #check if the alignment should be used
        if score < scoreCoff:
            lowScoreCntBA += 1
            continue
        if not overlap_test(qlen, slen, qAggrMatch, sAggrMatch, qLocMatch, sLocMatch, covCoff, overlapCoff,  debug=debug):
            overlapSkipCntBA += 1
            continue
        okCntBA += 1
        #now same the hit scores
        hitId = '%s!%s'%(q, s)
        hitIdRev = '%s!%s'%(s, q)
        #save the score
        scoreBA[hitId] = score
        #if hitRev is not present in AB
        if not hitIdRev in scoreAB:
            scoreAB[hitIdRev] = scoreCoff
        #add match and scores for each query sequence
        if not q in hitsBinA:
            hitsBinA[q] = OrderedDict()
        hitsBinA[q][s] = score

    # sort the AB hits by best score
    for query in hitsAinB:
        tmpDict = hitsAinB[query]
        if len(tmpDict) == 1:
            continue
        #update with the sorted ditionary
        tplList = [(k, tmpDict[k]) for k in sorted(tmpDict, key=tmpDict.get, reverse=True)]
        tmpDict = OrderedDict()
        for tpl in tplList:
            tmpDict[tpl[0]] = tpl[1]
        hitsAinB[query] = tmpDict
    # sort the BA hits by best score
    for query in hitsBinA:
        tmpDict = hitsBinA[query]
        if len(tmpDict) == 1:
            continue
        #update with the sorted ditionary
        tplList = [(k, tmpDict[k]) for k in sorted(tmpDict, key=tmpDict.get, reverse=True)]
        tmpDict = OrderedDict()
        for tpl in tplList:
            tmpDict[tpl[0]] = tpl[1]
        hitsBinA[query] = tmpDict
    if debug:
        print('Loaded hits AB:\t%d'%len(scoreAB))
        print('Query sequences in A with orthologs in B:\t%d'%len(hitsAinB))
        print('Overlap fail AB:\t%d'%overlapSkipCntAB)
        print('Low score AB:\t%d'%lowScoreCntAB)
        print('OK alingments AB:\t%d'%okCntAB)
        print('\nLoaded hits BA:\t%d'%len(scoreBA))
        print('Query sequences in B with orthologs in A:\t%d'%len(hitsBinA))
        print('Overlap fail BA:\t%d'%overlapSkipCntBA)
        print('Low score BA:\t%d'%lowScoreCntBA)
        print('OK alingments BA:\t%d'%okCntBA)
    #return the dictionaries
    return (scoreAB, hitsAinB, scoreBA, hitsBinA)



def load_besthits_between_proteomes(hitsAB, hitsBA, scoresAB, scoresBA, debug=False):
    """Load best hits for each query from AB and BA alignments."""
    if debug:
        print('load_besthits_between_proteomes :: START')
        print('Hits A in B: %d'%len(hitsAB))
        print('Hits B in A: %d'%len(hitsBA))
        print('Scores AB: %d'%len(scoresAB))
        print('Scores BA: %d'%len(scoresBA))
    bestHitsAB = OrderedDict()
    bestHitsBA = OrderedDict()
    bestscoreAB = OrderedDict()
    bestscoreBA = OrderedDict()
    greyZone = 0
    babHitsCnt = bbaHitsCnt = 0
    #calculate best hits for AB
    for q in hitsAB:
        qMatches = hitsAB[q]
        matches = list(qMatches.keys())
        mScores = list(qMatches.values())
        abHitId = '%s!%s'%(q, matches[0])
        bestScore = scoresAB[abHitId] #could be deleted!
        #### include both keys
        bestscoreAB[abHitId] = bestScore
        #add the match to the besthit dictionary for q
        bestHitsAB[q] = [matches[0]] #first match [the one with highest score]
        babHitsCnt += 1
        #start from the second hit in the matches list
        for i in range(1, len(matches)):
            tmpHitId = '%s!%s'%(q, matches[i])
            if (bestScore - scoresAB[tmpHitId] <= greyZone): #then add the correspondig match to the best matches
                bestHitsAB[q].append(matches[i])
                babHitsCnt += 1
            else: #otherwise exit the loop and go to next query
                break

    #calculate best hits for BA
    for q in hitsBA:
        qMatches = hitsBA[q]
        matches = list(qMatches.keys())
        mScores = list(qMatches.values())
        baHitId = '%s!%s'%(q, matches[0])
        bestScore = scoresBA[baHitId]
        #### include both keys
        bestscoreBA[baHitId] = bestScore
        #add the match to the besthit dictionary for q
        bestHitsBA[q] = [matches[0]] #first match [the one with highest score]
        bbaHitsCnt += 1
        #start from the second hit in the matches list
        for i in range(1, len(matches)):
            tmpHitId = '%s!%s'%(q, matches[i])
            if (bestScore - scoresBA[tmpHitId] <= greyZone): #then add the correspondig match to the best matches
                bestHitsBA[q].append(matches[i])
                bbaHitsCnt += 1
            else: #otherwise exit the loop and go to next query
                break
    if debug:
        print('Best hits loaded for AB:\t%d'%babHitsCnt)
        print('Best hits loaded for BA:\t%d'%bbaHitsCnt)
        print('bestscoresAB:\t%d'%len(bestscoreAB))
        print('bestscoresBA:\t%d'%len(bestscoreBA))
    return (bestHitsAB, bestHitsBA, bestscoreAB, bestscoreBA)



def load_within_proteomes_scores(alignFileAA, alignFileBB, ortoA, ortoB, covCoff=0.25, overlapCoff=0.5, scoreCoff=40, debug=False):
    """Load within proteomes scores for AA and BB alignments."""
    if debug:
        print('\nload_within_proteomes_scores :: START')
        print('Input AA: %s'%alignFileAA)
        print('Input BA: %s'%alignFileBB)
        print('Coverage cutoff: %s'%str(covCoff))
        print('Overlap cutoff: %s'%str(overlapCoff))
        print('Overlap cutoff: %s'%str(scoreCoff))
    if not os.path.isfile(alignFileAA):
        sys.stderr.write('The file %s was not found, please provide a valid input path'%alignFileAA)
        sys.exit(-2)
    if not os.path.isfile(alignFileBB):
        sys.stderr.write('The file %s was not found, please provide a valid input path'%alignFileBB)
        sys.exit(-2)
    #create the dictionaries to store the ids and scores
    maxMatchCnt = 1 #will stote the maximum number hit for all the processed alignments
    scoreAA = OrderedDict()
    scoreBB = OrderedDict()
    hitsAinA = OrderedDict()
    hitsBinB = OrderedDict()
    #counters to see what scores are kept
    lnCntAA = overlapSkipCntAA = lowScoreCntAA = okCntAA = isNotOrtoA = 0
    #start reading the scores from AA
    for ln in open(alignFileAA):
        lnCntAA += 1
        q, s, score, qlen, slen, qAggrMatch, sAggrMatch, qLocMatch, sLocMatch, dummy = ln.split('\t', 9)
        score = float(score)
        qlen = int(qlen)
        slen = int(slen)
        qAggrMatch = int(qAggrMatch)
        sAggrMatch = int(sAggrMatch)
        qLocMatch = int(qLocMatch)
        sLocMatch = int(sLocMatch)
        #check if the alignment should be used
        if score < scoreCoff:
            lowScoreCntAA += 1
            continue

        #skip if there if there an ortholog associated to the corresponding query sequence
        if q not in ortoA:
            isNotOrtoA += 1
            continue
        if not overlap_test(qlen, slen, qAggrMatch, sAggrMatch, qLocMatch, sLocMatch, covCoff, overlapCoff,  debug=debug):
            overlapSkipCntAA += 1
            continue
        okCntAA += 1
        #now add the hit and its score to the corresponding dictionaries
        hitId = '%s!%s'%(q, s)

        #save the score
        scoreAA[hitId] = int(round(score, 0))

        #add match and scores for each query sequence
        if not q in hitsAinA:
            hitsAinA[q] = OrderedDict()
        hitsAinA[q][s] = int(round(score, 0))

        matchCnt = len(hitsAinA[q])
        if matchCnt > maxMatchCnt:
            maxMatchCnt = matchCnt

    # start reading the scores from BB
    lnCntBB = overlapSkipCntBB = lowScoreCntBB = okCntBB = isNotOrtoB = 0
    for ln in open(alignFileBB):
        lnCntBB += 1
        q, s, score, qlen, slen, qAggrMatch, sAggrMatch, qLocMatch, sLocMatch, dummy = ln.split('\t', 9)
        score = float(score)
        qlen = int(qlen)
        slen = int(slen)
        qAggrMatch = int(qAggrMatch)
        sAggrMatch = int(sAggrMatch)
        qLocMatch = int(qLocMatch)
        sLocMatch = int(sLocMatch)
        #check if the alignment should be used
        if score < scoreCoff:
            lowScoreCntBB += 1
            continue
        #skip if there if there an ortholog associated to the corresponding query sequence
        if q not in ortoB:
            isNotOrtoB += 1
            continue
        if not overlap_test(qlen, slen, qAggrMatch, sAggrMatch, qLocMatch, sLocMatch, covCoff, overlapCoff,  debug=debug):
            overlapSkipCntBB += 1
            continue
        okCntBB += 1
        #now add the hit and its score to the corresponding dictionaries
        hitId = '%s!%s'%(q, s)
        #save the score
        scoreBB[hitId] = int(round(score, 0))
        #add match and scores for each query sequence
        if not q in hitsBinB:
            hitsBinB[q] = OrderedDict()
        hitsBinB[q][s] = int(round(score, 0))
        matchCnt = len(hitsBinB[q])
        if matchCnt > maxMatchCnt:
            maxMatchCnt = matchCnt

    if debug:
        print('\nSummary from analysis of within alignment:\t%s'%alignFileAA)
        print('Reads lines from AA:\t%d'%lnCntAA)
        print('Loaded hits for AA:\t%d'%len(scoreAA))
        print('Low score AA:\t%d'%lowScoreCntAA)
        print('Not a ortholog:\t%d'%isNotOrtoA)
        print('Overlap fail AA:\t%d'%overlapSkipCntAA)
        print('OK alingments AA:\t%d'%okCntAA)
        print('Query from A with hits in A:\t%d'%len(list(hitsAinA.keys())))
        print('\nSummary from analysis of within alignment:\t%s'%alignFileBB)
        print('Reads lines from BB:\t%d'%lnCntBB)
        print('Loaded hits for BB:\t%d'%len(scoreBB))
        print('Low score BB:\t%d'%lowScoreCntBB)
        print('Not a ortholog:\t%d'%isNotOrtoB)
        print('Overlap fail BB:\t%d'%overlapSkipCntBB)
        print('OK alingments BB:\t%d'%okCntBB)
        print('Query from B with hits in B:\t%d'%len(list(hitsBinB.keys())))
        print('Max maches count for AA and BB:\t%d'%maxMatchCnt)
    # return dictionaries
    return (scoreAA, hitsAinA, scoreBB, hitsBinB)



def preprocess_within_alignments_parallel(withinPreprocDict, alignDir, threads=4, covCoff=0.25, overlapCoff=0.5, scoreCoff=40, debug=False):
    """Preprocess the within alignments in parallel."""
    # withinPreprocDict contains dictionaries
    # with hits and scores for withing alignments

    # species names
    spList = list(withinPreprocDict.keys())
    # create the queue and start adding
    load_within_queue = mp.Queue(maxsize=len(spList) + threads)

    # fill the queue with the processes
    for sp in spList:
        pair = '{:s}-{:s}'.format(sp, sp)
        sys.stdout.flush()
        # add the complete path to the queue
        load_within_queue.put(os.path.join(alignDir, pair))

    # add flags for eneded jobs
    for i in range(0, threads):
        sys.stdout.flush()
        load_within_queue.put(None)

    # Queue to contain the execution time
    results_queue = mp.Queue(maxsize=len(spList))

    # call the method inside workers
    runningJobs = [mp.Process(target=consume_alignment_preproc, args=(load_within_queue, results_queue, withinPreprocDict, covCoff, overlapCoff, scoreCoff)) for i_ in range(threads)]

    for proc in runningJobs:
        proc.start()

    while True:
        try:
            spDone, preprocDictTmp, lenDictTmp = results_queue.get(False, 0.01)
            # add the information to the shared dictionary
            withinPreprocDict[spDone][1] = preprocDictTmp
            withinPreprocDict[spDone][2] = lenDictTmp
            if debug:
                sys.stdout.write('Preprocessing of within-alignment for {:s} done!\n'.format(spDone))
        except queue.Empty:
            pass
        allExited = True
        for t in runningJobs:
            if t.exitcode is None:
                allExited = False
                break
        if allExited & results_queue.empty():
            break

    # this joins the processes after we got the results
    for proc in runningJobs:
        while proc.is_alive():
            proc.join()



def postprocess_within_align(preprocWithinDict, ortoA, lenDictA, lenDictAbetween, debug=False):
    '''Proprocess within alignmets and create dictionary with scores and hits.'''
    if debug:
        print('postprocess_within_align :: START')
        print('Within align count: %d'%(len(preprocWithinDict)))
        print('Orthologs from A: {:d}'.format(len(ortoA)))
        print('Sequence lengths in between alignments: %d'%(len(lenDictAbetween)))

    #create the dictionaries to store the ids and scores
    scoreAA = OrderedDict()
    hitsAinA = OrderedDict()
    isNotOrtoA = okCntAA = 0

    # read the file and load hits for AA
    for hitid in preprocWithinDict:
        q, s = hitid.split('!', 1)
        #skip if there is an ortholog associated to the corresponding query sequence
        if q not in ortoA:
            isNotOrtoA += 1
            continue
        okCntAA += 1
        #now add the hit and its score to the corresponding dictionaries
        hitId = '{:s}!{:s}'.format(q, s)
        #save the score
        tmpScore = preprocWithinDict[hitid]
        scoreAA[hitId] = tmpScore
        if not q in lenDictAbetween:
            lenDictAbetween[q] = lenDictA[q]
        if s != q:
            if not s in lenDictAbetween:
                lenDictAbetween[s] = lenDictA[s]

        #add match and scores for each query sequence
        if not q in hitsAinA:
            hitsAinA[q] = OrderedDict()
        hitsAinA[q][s] = tmpScore

    #debug = True
    if debug:
        print('\nSequence lengths loaded for A:\t{:d}'.format(len(lenDictA)))
        print('Sequence lengths to be used for A:\t{:d}'.format(len(lenDictAbetween)))
        print('Loaded hits for AA:\t%d'%len(scoreAA))
        print('Not a ortholog:\t%d'%isNotOrtoA)
        print('OK alignments AA:\t%d'%okCntAA)
        print('Query from A with hits in A:\t%d'%len(list(hitsAinA.keys())))
    #sys.exit('DEBUG :: inpyranoid :: postprocess_within_align')
    # return dictionaries
    #return (scoreAA, hitsAinA, lenDictA)
    return (scoreAA, hitsAinA, lenDictAbetween)



def overlap_test(qlen, slen, qAggrMatch, sAggrMatch, qLocMatch, sLocMatch, coverageCutoff, overlapCutoff,  debug=False):
    """Filter out fragmentary hits."""
    # Filter out fragmentary hits by:
    # Ignore hit if aggregate matching area covers less than overlapCutoff of sequence.
    # Ignore hit if local matching segments cover less than coverageCutoff of sequence.
    #
    # qlen and slen are query and subject lengths, respectively
    # qAggrMatch and sAggrMatch are lengths of the aggregate matching region on query and subject. (From start of first matching segment to end of last matching segment).
    # qLocMatch and sLocMatch are local matching length on query and subject, (Sum of all segments length's on query [and subject]), respectively.
    # The above variables are respectively are found at positions 3-7 of parsed blast output lines
    #if qlen >= slen:
    if qAggrMatch < float(overlapCutoff * qlen):
        return False
    if qLocMatch < float(coverageCutoff * qlen):
        return False
    #else:
    if sAggrMatch < float(overlapCutoff * slen):
        return False
    if sLocMatch < float(coverageCutoff * slen):
        return False
    return True



def write_inpyranoid_output(pairClstrDict, ortoScoreDict, coreOrtoA, coreOrtoB, outName, outDir=os.getcwd(), writeTbl=True, writeRelations=False, debug=False):
    """Write output clusters for proteome pairs."""
    if debug:
        print('\nwrite_inpyranoid_output :: START')
        print('Proteome pair clusters: %s'%len(pairClstrDict))
        print('Dictionary with ortholog scores: %s'%len(ortoScoreDict))
        print('CORE ortholog for A: %s'%len(coreOrtoA))
        print('CORE ortholog for B: %s'%len(coreOrtoB))
        print('Ouput name suffix:\t%s'%outName)
        print('Ouput directory:\t%s'%outDir)
        print('Write table:\t%s'%writeTbl)
        print('Write relations:\t%s'%writeRelations)
    # check that the output name is in the correct format
    if not '-' in outName:
        print('ERROR: the output name must be a string with format species1-species2\n')
        sys.exit(-3)
    #extract species names
    sp1, sp2 = outName.split('-', 1)
    # set output paths
    # table output
    outTblPath = outTblFd = None
    if writeTbl:
        outTblPath = os.path.join(outDir, 'table.{:s}'.format(outName))
        outTblFd = open(outTblPath, 'w')
        outTblFd.write('OrtoId\tScore\tOrtoA\tOrtoB\n')
    # relations
    outRelPath = outRelFd = None
    if writeRelations:
        outRelPath = os.path.join(outDir, 'relations.{:s}'.format(outName))
        outRelFd = open(outRelPath, 'w')
    # sql
    outSqlPath = os.path.join(outDir, 'sqltable.{:s}'.format(outName))
    outSqlFd = open(outSqlPath, 'w')

    # write the files
    clstrCnt = 0
    for gPair in pairClstrDict:
        clstrCnt += 1
        clstrScore = ortoScoreDict[gPair]
        gA, gB = gPair.split('!', 1)
        tmpStr = ''
        # out line start (will be used for both table and sql output formats)
        outLnStart = '%d\t%d\t'%(clstrCnt, clstrScore)

        # write first part
        for gene, conf in pairClstrDict[gPair][gA].items():
            # NOTE: Maybe this never happens
            if conf < 1:
                if gene in coreOrtoA: # skip fake paralogs
                    continue
            if outTblFd:
                tmpStr += '%s %s '%(gene, str(conf))
            # write the sql file
            outSqlFd.write('%s%s\t%s\t%s\n'%(outLnStart, sp1, str(conf), gene))
        if outTblFd:
            outTblFd.write('%s%s\t'%(outLnStart, tmpStr[:-1]))
        # write the second part of the cluster
        tmpStr = ''
        for gene, conf in pairClstrDict[gPair][gB].items():
            if conf < 1:
                if gene in coreOrtoB: # skip fake paralogs
                    continue
            if outTblFd:
                tmpStr += '%s %s '%(gene, str(conf))
            # write the sql file
            outSqlFd.write('%s%s\t%s\t%s\n'%(outLnStart, sp2, str(conf), gene))
        if outTblFd:
            outTblFd.write('%s\n'%(tmpStr[:-1]))

    # close files
    outSqlFd.close()
    if outTblFd:
        outTblFd.close()
    if outRelFd:
        outRelFd.close()



def write_inpyranoid_output_simple(pairClstrDict, ortoScoreDict, coreOrtoA, coreOrtoB, outName, outDir=os.getcwd(), debug=False):
    """Write output clusters for proteome pairs."""
    if debug:
        print('\write_inpyranoid_output_simple :: START')
        print('Proteome pair clusters: %s'%len(pairClstrDict))
        print('Dictionary with ortholog scores: %s'%len(ortoScoreDict))
        print('CORE ortholog for A: %s'%len(coreOrtoA))
        print('CORE ortholog for B: %s'%len(coreOrtoB))
        print('Ouput name suffix:\t%s'%outName)
        print('Ouput directory:\t%s'%outDir)
    # check that the output name is in the correct format
    if not '-' in outName:
        print('ERROR: the output name must be a string with format species1-species2\n')
        sys.exit(-3)
    #extract species names
    sp1, sp2 = outName.split('-', 1)
    # set output paths
    # table output
    outTblPath = outTblFd = None

    outTblPath = os.path.join(outDir, 'table.{:s}'.format(outName))
    outTblFd = open(outTblPath, 'w')
    outTblFd.write('OrtoId\tScore\tOrtoA\tOrtoB\n')

    # write the files
    clstrCnt = 0
    for gPair in pairClstrDict:
        clstrCnt += 1
        clstrScore = ortoScoreDict[gPair]
        gA, gB = gPair.split('!', 1)
        tmpStr = ''
        # out line start (will be used for both table and sql output formats)
        outLnStart = '%d\t%d\t'%(clstrCnt, clstrScore)

        # write first part
        for gene, conf in pairClstrDict[gPair][gA].items():
            if conf < 1:
                if gene in coreOrtoA: # skip fake paralogs
                    continue
            tmpStr += '%s %s '%(gene, str(conf))
        outTblFd.write('%s%s\t'%(outLnStart, tmpStr[:-1]))
        # write the second part of the cluster
        tmpStr = ''
        for gene, conf in pairClstrDict[gPair][gB].items():
            if conf < 1:
                if gene in coreOrtoB: # skip fake paralogs
                    continue
            tmpStr += '%s %s '%(gene, str(conf))
        outTblFd.write('%s\n'%(tmpStr[:-1]))

    # close files
    outTblFd.close()

    # retun the path to the output file
    return outTblPath



# TODO: this part should be included in the function write_inpyranoid_output
# this would remove the need to write the clusters file 2 times
def load_clusters(inTbl, debug=False):
    '''Load clusters in dictionaries'''
    # will contain gene ids that where repeated
    # and will associate to each the set of cluster ids with the repetitions
    repeatDict = {}
    # will contain genes from A and B, to find repetitions
    aDict = {}
    bDict = {}
    # will have clstr ids as keys and the A, and B parts as values
    mergeableClstrs = {}

    # start reading the file
    ifd = open(inTbl, 'r')
    ifd.readline() # skip first line
    for ln in ifd:
        #clstrId, clstrSc, a, b = ln.rstrip('\n').split('\t', 3)
        clstrId, clstrSc, a, b = ln[:-1].split('\t', 3)
        # This cluster will be filtered later
        mergeableClstrs[clstrId] = [clstrSc, a, b]
        # check if the A genes are repeated
        tmpGenes = a.split(' ')
        for i, gene in enumerate(tmpGenes):
            if i % 2 == 0:
                # then it is a repetition
                if gene in aDict:
                    if not gene in repeatDict:
                        repeatDict[gene] = {aDict[gene], clstrId}
                    else:
                        repeatDict[gene].add(clstrId)
                else:
                    aDict[gene] = clstrId
        # check if the B genes are repeated
        tmpGenes = b.split(' ')
        # check if the B genes are peteated
        for i, gene in enumerate(tmpGenes):
            if i % 2 == 0:
                # then it is a repetition
                if gene in bDict:
                    if not gene in repeatDict:
                        repeatDict[gene] = {bDict[gene], clstrId}
                    else:
                        repeatDict[gene].add(clstrId)
                else:
                    bDict[gene] = clstrId
    ifd.close()

    return (repeatDict, mergeableClstrs)



def filter_mergeable_sets(repeatDict, mergeCandidateDict, debug=False):
    '''
    Merge sets to dictionaries.
    mergeCandidateDict contains cluster ids as keys,
    and as values, a list with, cluster score, part A, and part B of the cluster.
    '''
    # create string representations sets of clusters
    # and associate the size to each string repr
    setsDict = {}
    for k, val in repeatDict.items():
        #print(k, val)
        tmpStr = '_'.join(val)
        setsDict[tmpStr] = len(val)

    # SORT the dictionary by VALUE
    s = [(k, setsDict[k]) for k in sorted(setsDict, key=setsDict.get, reverse=True)]
    #setsDict.clear()
    del setsDict
    sList = []
    for k, v in s:
        #setsDict[k] = v
        # convert to a set of integers
        sList.append(set([int(x) for x in k.split('_')]))
    del s

    #sys.exit('DEBUG :: filter_mergeable_sets')

    # list to contain the sets
    toMerge = []
    notMerged = []
    # case in which only a set of cluster is avaliable
    # we just merge these clusters
    if len(sList) == 1:
        toMerge.append(sList[0])
    else:
        # first set (the biggest in size)
        while len(sList) > 1:
            firstSet = sList[0]
            toMerge.append(firstSet)
            #print(toMerge)
            for s in sList[1:]:
                # convert to set
                #print(str(s))
                #break
                if not s.issubset(firstSet):
                    notMerged.append(s)
            # update sList with the set of not merged sets
            sList.clear()
            sList = list(notMerged)
            #print(len(sList))
            notMerged.clear()
    #debug = True
    if debug:
        print('##### BEFORE PRUNING #####')
        print('toMerge len:\t{:d}'.format(len(toMerge)))
        print('##########################')

    # now process the sets to be merged
    # and make sure that intersections between the sets are empty
    sList.clear()
    sList = list(toMerge)

    #sys.exit('DEBUG :: filter_mergeable_sets')
    # contains a set as key and the interesction with another set as value
    intersectDict = {}
    # will contain idx in toMerge list of possible new subsets
    newSubSets = {}
    iterCnt = 0
    if debug:
        print('##### START PRUNING #####')
    while len(sList) > 1:
        iterCnt += 1
        firstSet = sList[0]
        if debug:
            print('Iteration:\t{:d}'.format(iterCnt))
            print('First set:\t{:s}'.format(str(firstSet)))
        #toMerge.append(firstSet)
        #print(toMerge)
        #sys.exit('DEBUG')
        for idx, s in enumerate(sList[1:]):
            if len(firstSet) == 1:
                if debug:
                    print('Skip this set...first set is one...')
                break
            elif len(s) == 1:
                if debug:
                    print('Skip this 1-element set comparison...go to next set...')
                continue

            # calculate the intersections
            intersection = s.intersection(firstSet)
            # check if there are common elements
            if len(intersection) > 0:
                if s.issubset(firstSet):
                    if debug:
                        print('Skip this set {:s}...is a subset of a bigger set...'.format(str(s)))
                    newSubIdx = iterCnt + idx
                    newSubSets[newSubIdx] = None
                    continue

                # create a string repr of the set
                sStr = '_'.join([str(x) for x in s])
                if not sStr in intersectDict:
                    intersectDict[sStr] = intersection
                    # find the index in the toMerge list
                    toMergeIdx = iterCnt + idx
                    #print('\n{:d}\t{:s}\t{:s}'.format(toMergeIdx, sStr, str(intersectDict[sStr])))
                    # remove the cluster with intersections
                    if debug:
                        print('Set before updated:\t{:s}'.format(str(toMerge[toMergeIdx])))
                    toMerge[toMergeIdx] = toMerge[toMergeIdx].difference(intersection)
                    if debug:
                        print('Updated set:\t{:s}'.format(str(toMerge[toMergeIdx])))
                else:
                    if debug:
                        print('WARNING: The set {:s} was found to have multiple intersections'.format(str(s)))
                        print('it will be skipped...')
                    skipIdx = iterCnt + idx
                    newSubSets[skipIdx] = None

        # update sList with the set of not merged sets
        sList.clear()
        sList = list(toMerge[iterCnt:])
        if debug:
            print('\nRemaining iterations:\t{:d}'.format(len(sList)))
            print('#####################')
        #sys.exit('DEBUG')

    if debug:
        print('\n########## PRUNING DONE ###########')
        print('\ntoMerge before removing single element sets:\t{:d}'.format(len(toMerge)))

    # remove the element with a single cluster from the merge list
    # clusters which could not be merged with other...
    # maybe remove these????
    #skipList = []
    skipDict = {}
    tmpList = list(toMerge)
    for i, el in enumerate(tmpList):
        if len(el) == 1:
            #skipList.append(el)
            for val in el:
                if not str(val) in skipDict:
                    skipDict[str(val)] = None
            # remove from toMerge
            toMerge.remove(el)
        elif i in newSubSets:
            #skipList.append(el)
            for val in el:
                if not str(val) in skipDict:
                    skipDict[str(val)] = None
            # remove from toMerge
            toMerge.remove(el)

    #debug = False
    if debug:
        print('\ntoMerge final:\t{:d}'.format(len(toMerge)))
        #print('Not mergeable clusters:\t{:d}'.format(len(skipList)))
        print('Clusters to be removed:\t{:d}'.format(len(skipDict)))

    #sys.exit('debug :: filter_mergeable_sets')

    # will contain a subset of the mergeCandidateDict
    mergeableFinal = {}
    # make sure that all the mergeable clusters appear only one time
    tmpDict = {}
    # just an extra check, but should never happen!
    for s in toMerge:
        for val in s:
            if not val in tmpDict:
                tmpDict[val] = None
                # add cluster to the final dictionary with mergeable clusters
                tmpClstrId = str(val)
                mergeableFinal[tmpClstrId] = mergeCandidateDict[tmpClstrId]
            else:
                print('ERROR: The cluster ID {:s} was found multiple times in the sets to be merged'.format(str(val)))
                sys.exit(-5)
    #sys.exit('debug :: filter_mergeable_sets')
    return (toMerge, mergeableFinal, skipDict)



def merge_clusters(toMergeSetList, mergeableClusters):
    '''Will generate strings reprenting the merged clusters'''
    # will contain the merged clusters
    mergedStack = []
    # will contain the ids of the merged clusters
    mergedClstrList = []

    for s in toMergeSetList:
        # for each cluster the genes from A and B will loaded
        # in dictionaries, that will be sorted by max score
        # the average cluster score will be computed
        scList = []
        tmpA = {}
        tmpB = {}
        for clstrId in s:
            mergedClstrList.append(str(clstrId))
            sc, a, b = mergeableClusters[str(clstrId)]
            scList.append(int(sc))
            # split the A part and add the elements in tmpA dict
            tmpGenes = a.split(' ')
            for i, gene in enumerate(tmpGenes):
                if i % 2 == 0:
                    # add the gene in the dictionary
                    if not gene in tmpA:
                        tmpA[gene] = round(float(tmpGenes[i + 1]), 3)
            # split the B part and add the elements in tmpB dict
            tmpGenes = b.split(' ')
            for i, gene in enumerate(tmpGenes):
                if i % 2 == 0:
                    # add the gene in the dictionary
                    if not gene in tmpB:
                        tmpB[gene] = round(float(tmpGenes[i + 1]), 3)
        # print info about the merged clusters
        avgSc = str(int(array(scList).mean()))

        # sort the dictionary by value
        tmpSort = [(k, tmpA[k]) for k in sorted(tmpA, key=tmpA.get, reverse=True)]
        tmpA.clear()
        strA = ''
        for k, v in tmpSort:
            strA += '{:s} {:s} '.format(k, str(v))
        del tmpSort

        # sort the dictionary by value
        tmpSort = [(k, tmpB[k]) for k in sorted(tmpB, key=tmpB.get, reverse=True)]
        tmpB.clear()
        strB = ''
        for k, v in tmpSort:
            strB += '{:s} {:s} '.format(k, str(v))
        del tmpSort

        # remove the final space
        strA = strA[:-1]
        strB = strB[:-1]

        #finalStr = '{:s}\t{:s}\t{:s}\n'.format(avgSc, strA, strB)
        #mergedStack.append(finalStr)
        mergedStack.append('{:s}\t{:s}\t{:s}\n'.format(avgSc, strA, strB))
    return (mergedStack, mergedClstrList)



def rewrite_clusters(inTbl, mergedClstrs, mergedIds, skipDict):
    '''Write the cluster tables with the merged clusters'''
    # main directory
    rootDir = os.path.dirname(inTbl)
    #tmpPath = os.path.join('{:s}.tmp'.format(inTbl))
    tmpPath = os.path.join('{:s}.tmp'.format(inTbl.rsplit('.', 1)[0]))

    ifd = open(inTbl, 'r')
    # extract the hdr
    hdr = ifd.readline()
    tmpfd = open(tmpPath, 'w')
    tmpfd.write('{:s}'.format(hdr.split('\t', 1)[-1]))

    oldClstrCnt = 0
    for ln in ifd:
        oldClstrCnt += 1
        clstrId, rxPart = ln.split('\t', 1)
        # skip if the id is in skip dictionary
        if clstrId in skipDict:
            continue
        if not clstrId in mergedIds:
            #tmpfd.write(ln.split('\t', 1)[-1])
            tmpfd.write(rxPart)
    # now write the merged clusters in the new table
    while len(mergedClstrs) > 0:
        tmpfd.write(mergedClstrs.pop())
    # close files
    ifd.close()
    tmpfd.close()

    # load the new table using pandas and sort it by clstr score
    df = pd.read_csv(tmpPath, sep='\t')
    # sort the dataframe by score
    dfSorted = df.sort_values('Score', ascending=False, inplace=False)
    del df

    #tmpSorted = '{:s}.sorted'.format(inTbl)
    tmpSorted = tmpPath.replace('.tmp', '.sorted')
    dfSorted.to_csv(tmpSorted, sep='\t', header=False, index=False)

    ''' REMOVE IT INSTEAD
    # rename the original table adding .old at the end of its name
    os.rename(inTbl, '{:s}.old'.format(inTbl))
    '''
    os.remove(inTbl)

    finalOutName = os.path.basename(inTbl.rsplit('.', 1)[0])
    finalTblPath = os.path.join(os.path.dirname(inTbl), finalOutName)

    # extract species names
    #sp1, sp2 = inTbl.split('.', 1)[-1].split('-', 1)
    sp1, sp2 = finalOutName.split('.', 1)[-1].split('-', 1)

    # write the final files
    # SQL table
    #sqlTbl = 'sql{:s}'.format(os.path.basename(inTbl))
    sqlTbl = 'sql{:s}'.format(finalOutName)
    sqlTbl = os.path.join(os.path.dirname(inTbl), sqlTbl)

    # create SQL table
    ofdSql = open(sqlTbl, 'w')
    # main table
    #ofd = open(inTbl, 'w')
    ofd = open(finalTblPath, 'w')
    ofd.write(hdr)
    tmpCnt = 0
    for ln in open(tmpSorted, 'r'):
        tmpCnt += 1
        ofd.write('{:d}\t{:s}'.format(tmpCnt, ln))
        # extract the parts to be written in the SQl file
        avgSc, a, b = ln[:-1].split('\t', 2)
        outSqlLnStart = '{:d}\t{:s}'.format(tmpCnt, avgSc)

        # write genes from Sp1 in SQL file
        tmpGenes = a.split(' ')
        for i, el in enumerate(tmpGenes):
            # if it is the gene part
            if i % 2 == 0:
                ofdSql.write('{:s}\t{:s}\t{:s}\t{:s}\n'.format(outSqlLnStart, sp1, tmpGenes[i + 1], el))

        # write genes from Sp2 in SQL file
        tmpGenes = b.split(' ')
        for i, el in enumerate(tmpGenes):
            # if it is the gene part
            if i % 2 == 0:
                ofdSql.write('{:s}\t{:s}\t{:s}\t{:s}\n'.format(outSqlLnStart, sp2, tmpGenes[i + 1], el))

    ofd.close()
    ofdSql.close()
    # remove the tmp files
    os.remove(tmpPath)
    os.remove(tmpSorted)
    # return the new path, old clstr count, and new clstr cnt
    #sys.exit('DEBUG :: rewrite_clusters')
    # NOTE: sorting using pandas and adding a new column created some problems
    return(inTbl, sqlTbl)



def merge_and_write_inpyranoid_output(inTbl, debug=False):
    '''Read a table, merge clusters, and write sql table.'''
    repeatDict, mergeableDict = load_clusters(inTbl, debug=debug)
    # reduce sets of mergeable clusters
    toMergeSetList, mergeableFinalDict, skipDict = filter_mergeable_sets(repeatDict, mergeableDict, debug=debug)
    del mergeableDict # not needed anymore
    # merge the clusters
    mergeStack, mergedIds = merge_clusters(toMergeSetList, mergeableFinalDict)
    # rewrite cluster table (if required)
    #if len(mergeStack) > 0:
    inTbl, sqlTbl = rewrite_clusters(inTbl, mergeStack, mergedIds, skipDict)
