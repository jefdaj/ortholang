from libc.stdio cimport *
from libc.stdlib cimport atoi
from libc.stdlib cimport atof
import sys
import os
from collections import OrderedDict


cdef extern from "stdio.h":
    #FILE * fopen ( const char * filename, const char * mode )
    FILE *fopen(const char *, const char *)
    #int fclose ( FILE * stream )
    int fclose(FILE *)
    #ssize_t getline(char **lineptr, size_t *n, FILE *stream);
    ssize_t getline(char **, size_t *, FILE *)



def equalize_AB_and_BA_scores_fast(abScores, baScores, debug=False):
    """Equalize the scores from AB and BA alignments."""
    # for example given the hit g-c in AB with score x,
    # and given the hits c-g in BA with score y
    # a uniq values for BA[c-g] and AB[g-c] are calculated and the corresponding
    # dictionaries updated accordingly
    if debug:
        print('equalize_AB_and_BA_scores_fast :: START')
        print('Hits for AB: %d'%len(abScores))
        print('Hits for BA: %d'%len(baScores))
    # check input lengths
    if len(abScores) != len(baScores):
        sys.write.stderr('ERROR: the two dictionaries must contain the same number of hits')
        sys.exit(-5)

    # temporary variables
    cdef float scAB, scBA
    cdef int avgScore

    #start looping the hits in AB
    for k in abScores:
        scAB = abScores[k]
        #get revese key
        q, s = k.split('!', 1)
        k2 = '%s!%s'%(s, q)
        scBA = baScores[k2]
        #calculate the average value
        #correct rounding
        #avgScore = int(round(float((scAB + scBA) / 2.), 0))
        #print(avgScore)
        avgScore = <int>round(float((scAB + scBA) / 2.), 0)
        #print(avgScore)
        abScores[k] = baScores[k2] = avgScore
    return (abScores, baScores)



def load_between_proteomes_scores_fast(alignFileAB, alignFileBA, covCoff=0.25, overlapCoff=0.5, scoreCoff=40, debug=False):
    """Load between proteomes scores for AB and BA alignments (Cython)."""
    if debug:
      print('load_between_proteomes_scores_fast :: START')
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
    # will contain the sequence lengths
    betweenLenDictA = OrderedDict() # only A related genes
    betweenLenDictB = OrderedDict() # only B related genes

    # define file names and file descriptor pointer in C
    filename_byte_string = alignFileAB.encode("UTF-8")
    cdef char* alignFileABc = filename_byte_string
    # convert the second input file name to bytestring
    filename_byte_string = alignFileBA.encode("UTF-8")
    cdef char* alignFileBAc = filename_byte_string
    #file pointer
    cdef FILE* cfile
    # varibales for files and lines
    cdef char * line = NULL
    cdef size_t l = 0
    cdef ssize_t read

    # define the variables that will be used
    cdef int qlen, slen, qAggrMatch, sAggrMatch, qLocMatch, sLocMatch, lnCntAB, overlapSkipCntAB, lowScoreCntAB, okCntAB
    cdef float score, overlapCoff_c, scoreCoff_c, covCoff_c
    covCoff_c = <float>covCoff
    scoreCoff_c = <float>scoreCoff
    overlapCoff_c = <float>overlapCoff
    #initialize counters
    lnCntAB = 0
    overlapSkipCntAB = 0
    lowScoreCntAB = 0
    okCntAB = 0

    #open the alignments for AB
    cfile = fopen(alignFileABc, "rb")
    #if cfile == NULL:
        #raise FileNotFoundError(2, "No such file or directory: '%s'" % alignFileABc)
    # read the file and load hits for AA
    while True:
        read = getline(&line, &l, cfile)
        if read == -1:
            break

        #split the string
        flds = line.split(b'\t', 8)
        # convert bytes to strings
        q = flds[0].decode()
        s = flds[1].decode()
        score = atof(flds[2])
        qlen = atoi(flds[3])
        slen = atoi(flds[4])
        qAggrMatch = atoi(flds[5])
        sAggrMatch = atoi(flds[6])
        qLocMatch = atoi(flds[7])
        sLocMatch = atoi(flds[8])
        #check if the alignment should be used
        ''' This should never happen!
        if score < scoreCoff_c:
            lowScoreCntAB += 1
            continue
        '''
        if overlap_test_c(qlen, slen, qAggrMatch, sAggrMatch, qLocMatch, sLocMatch, covCoff_c, overlapCoff_c) == 1:
            overlapSkipCntAB += 1
            #if s == 'G0MEW7':
              #print(line)
              #sys.exit('DEBUG :: load_between_proteomes_scores_fast')
            continue
        okCntAB += 1
        #now same the hit scores
        hitId = '%s!%s'%(q, s)
        hitIdRev = '%s!%s'%(s, q)
        # add sequence lengths in dictionary
        if not q in betweenLenDictA:
          betweenLenDictA[q] = qlen
        if not s in betweenLenDictB:
          betweenLenDictB[s] = slen
        #save the score
        scoreAB[hitId] = score
        #add match and scores for each query sequence
        if not q in hitsAinB:
            hitsAinB[q] = OrderedDict()
        hitsAinB[q][s] = score
        #initialize the mutual score to the score cut-off
        scoreBA[hitIdRev] = scoreCoff
    #close input file
    fclose(cfile)

    # define the variables that will be used for BB
    cdef int overlapSkipCntBB, lowScoreCntBB, okCntBB
    #initialize counters
    overlapSkipCntBA = 0
    lowScoreCntBA = 0
    okCntBA = 0

    #open the alignments for AB
    cfile = fopen(alignFileBAc, "rb")
    #if cfile == NULL:
        #raise FileNotFoundError(2, "No such file or directory: '%s'" % alignFileBAc)
    # read the file and load hits for AA
    while True:
        read = getline(&line, &l, cfile)
        if read == -1:
            break

        #split the string
        flds = line.split(b'\t', 8)
        # convert bytes to strings
        q = flds[0].decode()
        s = flds[1].decode()
        score = atof(flds[2])
        qlen = atoi(flds[3])
        slen = atoi(flds[4])
        qAggrMatch = atoi(flds[5])
        sAggrMatch = atoi(flds[6])
        qLocMatch = atoi(flds[7])
        sLocMatch = atoi(flds[8])

        #check if the alignment should be used
        ''' This should never happen!
        if score < scoreCoff_c:
            lowScoreCntBA += 1
            continue
        '''
        if overlap_test_c(qlen, slen, qAggrMatch, sAggrMatch, qLocMatch, sLocMatch, covCoff_c, overlapCoff_c) == 1:
            overlapSkipCntBA += 1
            continue
        okCntBA += 1
        #now same the hit scores
        hitId = '%s!%s'%(q, s)
        hitIdRev = '%s!%s'%(s, q)
        # add sequence lengths in dictionaries
        if not q in betweenLenDictB:
          betweenLenDictB[q] = qlen
        if not s in betweenLenDictA:
          betweenLenDictA[s] = slen
        #save the score
        scoreBA[hitId] = score
        #if hitRev is not present in AB
        if not hitIdRev in scoreAB:
            scoreAB[hitIdRev] = scoreCoff
        #add match and scores for each query sequence
        if not q in hitsBinA:
            hitsBinA[q] = OrderedDict()
        hitsBinA[q][s] = score
    #close input file
    fclose(cfile)

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
        print('Loaded sequence lengths for A:\t{:d}'.format(len(betweenLenDictA)))
        print('Loaded sequence lengths for B:\t{:d}'.format(len(betweenLenDictB)))
        print('\nLoaded hits AB:\t%d'%len(scoreAB))
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
    return (scoreAB, hitsAinB, scoreBA, hitsBinA, betweenLenDictA, betweenLenDictB)



def load_within_proteomes_scores_fast(alignFileAA, alignFileBB, ortoA, ortoB, lenDictA, lenDictB, covCoff=0.25, overlapCoff=0.5, scoreCoff=40, debug=False):
    """Load within proteomes scores for AA and BB alignments (Cython)."""
    if debug:
        print('\nload_within_proteomes_scores_fast (Cython) :: START')
        print('Input AA: %s'%alignFileAA)
        print('Input BA: %s'%alignFileBB)
        print('Sequence lengths pre-loaded fo A: {:d}'.format(len(lenDictA)))
        print('Sequence lengths pre-loaded fo B: {:d}'.format(len(lenDictB)))
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
    #maxMatchCnt = 1 #will stote the maximum number hit for all the processed alignments
    scoreAA = OrderedDict()
    scoreBB = OrderedDict()
    hitsAinA = OrderedDict()
    hitsBinB = OrderedDict()

    # define file names and file descriptor pointer in C
    filename_byte_string = alignFileAA.encode("UTF-8")
    cdef char* alignFileAAc = filename_byte_string
    # convert the second input file name to bytestring
    filename_byte_string = alignFileBB.encode("UTF-8")
    cdef char* alignFileBBc = filename_byte_string
    #file pointer
    cdef FILE* cfile
    # varibales for files and lines
    cdef char * line = NULL
    cdef size_t l = 0
    cdef ssize_t read

    # define the variables that will be used
    cdef int qlen, slen, qAggrMatch, sAggrMatch, qLocMatch, sLocMatch, maxMatchCnt, lnCntAA, overlapSkipCntAA, lowScoreCntAA, okCntAA, isNotOrtoA

    cdef float score, overlapCoff_c, scoreCoff_c, covCoff_c
    covCoff_c = <float>covCoff
    scoreCoff_c = <float>scoreCoff
    overlapCoff_c = <float>overlapCoff
    #initialize counters
    lnCntAA = 0
    overlapSkipCntAA = 0
    lowScoreCntAA = 0
    okCntAA = 0
    isNotOrtoA = 0

    #open the alignments for AA
    cfile = fopen(alignFileAAc, "rb")
    if cfile == NULL:
        raise FileNotFoundError(2, "No such file or directory: '%s'" % alignFileAAc)
    # read the file and load hits for AA
    while True:
        read = getline(&line, &l, cfile)
        if read == -1:
            break
        lnCntAA += 1
        #split the string
        flds = line.split(b'\t', 8)

        # convert bytes to strings
        #use cfuntions for convertions to int and float
        q = flds[0].decode()
        #print(q)
        s = flds[1].decode()
        #print(s)
        score = atof(flds[2])
        qlen = atoi(flds[3])
        slen = atoi(flds[4])
        qAggrMatch = atoi(flds[5])
        sAggrMatch = atoi(flds[6])
        qLocMatch = atoi(flds[7])
        sLocMatch = atoi(flds[8])

        #check if the alignment should be used
        if score < scoreCoff_c:
            lowScoreCntAA += 1
            continue
        #skip if there if there an ortholog associated to the corresponding query sequence
        if q not in ortoA:
            isNotOrtoA += 1
            continue
        if overlap_test_c(qlen, slen, qAggrMatch, sAggrMatch, qLocMatch, sLocMatch, covCoff_c, overlapCoff_c) == 1:
            overlapSkipCntAA += 1
            continue
        okCntAA += 1
        #now add the hit and its score to the corresponding dictionaries
        hitId = '%s!%s'%(q, s)
        # add sequence lengths in dictionary
        if not q in lenDictA:
          lenDictA[q] = qlen
        if q != s:
          if not s in lenDictA:
            lenDictA[s] = slen
        #save the score
        scoreAA[hitId] = int(round(score, 0))
        #add match and scores for each query sequence
        if not q in hitsAinA:
            hitsAinA[q] = OrderedDict()
        hitsAinA[q][s] = int(round(score, 0))

    #close input file
    fclose(cfile)

    # define the variables that will be used for BB
    cdef int lnCntBB, overlapSkipCntBB, lowScoreCntBB, okCntBB, isNotOrtoB

    #initialize counters
    lnCntBB = 0
    overlapSkipCntBB = 0
    lowScoreCntBB = 0
    okCntBB = 0
    isNotOrtoB = 0

    #open the alignments for BB
    cfile = fopen(alignFileBBc, "rb")
    if cfile == NULL:
        raise FileNotFoundError(2, "No such file or directory: '%s'" % alignFileBBc)
    # read the file and load hits for AA
    while True:
        read = getline(&line, &l, cfile)
        if read == -1:
            break
        lnCntBB += 1
        #split the string
        flds = line.split(b'\t', 8)
        # convert bytes to strings
        #use cfuntions for convertions to int and float
        q = flds[0].decode()
        s = flds[1].decode()
        score = atof(flds[2])
        qlen = atoi(flds[3])
        slen = atoi(flds[4])
        qAggrMatch = atoi(flds[5])
        sAggrMatch = atoi(flds[6])
        qLocMatch = atoi(flds[7])
        sLocMatch = atoi(flds[8])

        #check if the alignment should be used
        if score < scoreCoff_c:
            lowScoreCntBB += 1
            continue
        #skip if there if there an ortholog associated to the corresponding query sequence
        if q not in ortoB:
            isNotOrtoB += 1
            continue
        if overlap_test_c(qlen, slen, qAggrMatch, sAggrMatch, qLocMatch, sLocMatch, covCoff_c, overlapCoff_c) == 1:
            overlapSkipCntBB += 1
            continue
        okCntBB += 1
        #now add the hit and its score to the corresponding dictionaries
        hitId = '%s!%s'%(q, s)
        # add sequence lengths in dictionary
        if not q in lenDictB:
          lenDictB[q] = qlen
        if q != s:
          if not s in lenDictB:
            lenDictB[s] = slen
        #save the score
        scoreBB[hitId] = int(round(score, 0))
        #add match and scores for each query sequence
        if not q in hitsBinB:
            hitsBinB[q] = OrderedDict()
        hitsBinB[q][s] = int(round(score, 0))

    #close input file
    fclose(cfile)
    #print some debug lines
    if debug:
        print('Sequence lengths loaded fo A: {:d}'.format(len(lenDictA)))
        print('Sequence lengths loaded fo B: {:d}'.format(len(lenDictB)))
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
        #print('Max maches count for AA and BB:\t%d'%maxMatchCnt)
    # return dictionaries
    return (scoreAA, hitsAinA, scoreBB, hitsBinB, lenDictA, lenDictB)



def load_within_proteomes_scores_single(alignFileAA, ortoA, lenDictA, covCoff=0.25, overlapCoff=0.5, scoreCoff=40, debug=False):
    """Load within proteomes scores for AA and BB alignments (Cython)."""
    if debug:
        printf('\nload_within_proteomes_scores_single (Cython) :: START')
        print('Input AA: %s'%alignFileAA)
        print('Sequence lengths pre-loaded for A: {:d}'.format(len(lenDictA)))
        print('Coverage cutoff: %s'%str(covCoff))
        print('Overlap cutoff: %s'%str(overlapCoff))
        print('Overlap cutoff: %s'%str(scoreCoff))
    if not os.path.isfile(alignFileAA):
        sys.stderr.write('The file %s was not found, please provide a valid input path'%alignFileAA)
        sys.exit(-2)

    #create the dictionaries to store the ids and scores
    scoreAA = OrderedDict()
    hitsAinA = OrderedDict()

    # define file names and file descriptor pointer in C
    filename_byte_string = alignFileAA.encode("UTF-8")
    cdef char* alignFileAAc = filename_byte_string
    #file pointer
    cdef FILE* cfile
    # varibales for files and lines
    cdef char * line = NULL
    cdef size_t l = 0
    cdef ssize_t read

    # define the variables that will be used
    cdef int qlen, slen, qAggrMatch, sAggrMatch, qLocMatch, sLocMatch, maxMatchCnt, lnCntAA, overlapSkipCntAA, lowScoreCntAA, okCntAA, isNotOrtoA

    cdef float score, overlapCoff_c, scoreCoff_c, covCoff_c
    covCoff_c = <float>covCoff
    scoreCoff_c = <float>scoreCoff
    overlapCoff_c = <float>overlapCoff
    #initialize counters
    lnCntAA = 0
    overlapSkipCntAA = 0
    lowScoreCntAA = 0
    okCntAA = 0
    isNotOrtoA = 0

    #open the alignments for AA
    cfile = fopen(alignFileAAc, "rb")
    if cfile == NULL:
        raise FileNotFoundError(2, "No such file or directory: '%s'" % alignFileAAc)
    # read the file and load hits for AA
    while True:
        read = getline(&line, &l, cfile)
        if read == -1:
            break
        lnCntAA += 1
        #split the string
        flds = line.split(b'\t', 8)

        # convert bytes to strings
        #use cfuntions for convertions to int and float
        q = flds[0].decode()
        #print(q)
        s = flds[1].decode()
        #print(s)
        score = atof(flds[2])
        qlen = atoi(flds[3])
        slen = atoi(flds[4])
        qAggrMatch = atoi(flds[5])
        sAggrMatch = atoi(flds[6])
        qLocMatch = atoi(flds[7])
        sLocMatch = atoi(flds[8])

        #check if the alignment should be used
        if score < scoreCoff_c:
            lowScoreCntAA += 1
            continue
        #skip if there is an ortholog associated to the corresponding query sequence
        if q not in ortoA:
            isNotOrtoA += 1
            continue
        if overlap_test_c(qlen, slen, qAggrMatch, sAggrMatch, qLocMatch, sLocMatch, covCoff_c, overlapCoff_c) == 1:
            overlapSkipCntAA += 1
            continue
        okCntAA += 1
        #now add the hit and its score to the corresponding dictionaries
        hitId = '%s!%s'%(q, s)
        # add sequence lengths in dictionary
        if not q in lenDictA:
          lenDictA[q] = qlen
        if q != s:
          if not s in lenDictA:
            lenDictA[s] = slen
        #save the score
        scoreAA[hitId] = int(round(score, 0))
        #add match and scores for each query sequence
        if not q in hitsAinA:
            hitsAinA[q] = OrderedDict()
        hitsAinA[q][s] = int(round(score, 0))

    #close input file
    fclose(cfile)

    #print some debug lines
    if debug:
        print('Sequence lengths loaded fo A: {:d}'.format(len(lenDictA)))
        print('\nSummary from analysis of within alignment:\t%s'%alignFileAA)
        print('Reads lines from AA:\t%d'%lnCntAA)
        print('Loaded hits for AA:\t%d'%len(scoreAA))
        print('Low score AA:\t%d'%lowScoreCntAA)
        print('Not a ortholog:\t%d'%isNotOrtoA)
        print('Overlap fail AA:\t%d'%overlapSkipCntAA)
        print('OK alignments AA:\t%d'%okCntAA)
        print('Query from A with hits in A:\t%d'%len(list(hitsAinA.keys())))
    # return dictionaries
    return (scoreAA, hitsAinA, lenDictA)



def preprocess_within_align(alignFileAA, lenDictA, covCoff=0.25, overlapCoff=0.5, scoreCoff=40, debug=False):
    """Load within proteomes scores for AA and BB alignments (Cython)."""
    if debug:
        print('\npreprocess_within_align (Cython) :: START')
        print('Input AA: %s'%alignFileAA)
        print('Coverage cutoff: %s'%str(covCoff))
        print('Overlap cutoff: %s'%str(overlapCoff))
        print('Overlap cutoff: %s'%str(scoreCoff))
    if not os.path.isfile(alignFileAA):
        sys.stderr.write('The file %s was not found, please provide a valid input path'%alignFileAA)
        sys.exit(-2)

    #create the dictionaries to store the ids and scores
    # Will contain the score for each query-subject pair
    withinAlignDict = {}

    # define file names and file descriptor pointer in C
    filename_byte_string = alignFileAA.encode("UTF-8")
    cdef char* alignFileAAc = filename_byte_string
    #file pointer
    cdef FILE* cfile
    # varibales for files and lines
    cdef char * line = NULL
    cdef size_t l = 0
    cdef ssize_t read

    # define the variables that will be used
    cdef int qlen, slen, qAggrMatch, sAggrMatch, qLocMatch, sLocMatch, maxMatchCnt, lnCntAA, overlapSkipCntAA, lowScoreCntAA, okCntAA, isNotOrtoA

    cdef float score, overlapCoff_c, scoreCoff_c, covCoff_c
    covCoff_c = <float>covCoff
    scoreCoff_c = <float>scoreCoff
    overlapCoff_c = <float>overlapCoff
    #initialize counters
    lnCntAA = 0
    overlapSkipCntAA = 0
    lowScoreCntAA = 0
    okCntAA = 0
    isNotOrtoA = 0

    #open the alignments for AA
    cfile = fopen(alignFileAAc, "rb")
    if cfile == NULL:
        raise FileNotFoundError(2, "No such file or directory: '%s'" % alignFileAAc)
    # read the file and load hits for AA
    while True:
        read = getline(&line, &l, cfile)
        if read == -1:
            break
        lnCntAA += 1
        #split the string
        flds = line.split(b'\t', 8)

        # convert bytes to strings
        #use cfuntions for convertions to int and float
        q = flds[0].decode()
        #print(q)
        s = flds[1].decode()
        #print(s)
        score = atof(flds[2])
        qlen = atoi(flds[3])
        slen = atoi(flds[4])
        qAggrMatch = atoi(flds[5])
        sAggrMatch = atoi(flds[6])
        qLocMatch = atoi(flds[7])
        sLocMatch = atoi(flds[8])

        #check if the alignment should be used
        '''
        if score < scoreCoff_c:
            lowScoreCntAA += 1
            continue
        '''
        #skip if there is an ortholog associated to the corresponding query sequence
        '''
        if q not in ortoA:
            isNotOrtoA += 1
            continue
        '''
        if overlap_test_c(qlen, slen, qAggrMatch, sAggrMatch, qLocMatch, sLocMatch, covCoff_c, overlapCoff_c) == 1:
            overlapSkipCntAA += 1
            continue
        okCntAA += 1
        #now add the hit and its score to the corresponding dictionaries
        hitId = '%s!%s'%(q, s)

        if not hitId in withinAlignDict:
          withinAlignDict[hitId] = int(round(score, 0))

        # add sequence lengths in dictionary
        #'''
        if not q in lenDictA:
          lenDictA[q] = qlen
        if q != s:
          if not s in lenDictA:
            lenDictA[s] = slen
        #'''

    #close input file
    fclose(cfile)

    #print some debug lines
    if debug:
        print('\nSummary from preprocessing of within alignment:\t%s'%alignFileAA)
        print('Reads lines from AA:\t%d'%lnCntAA)
        print('Sequence lengths loaded for A:\t{:d}'.format(len(lenDictA)))
        print('Overlap fail AA:\t%d'%overlapSkipCntAA)
        print('Low score AA:\t%d'%lowScoreCntAA)
        print('Entries in which alignment dict:\t%d'%len(withinAlignDict))
    # return dictionaries
    return (withinAlignDict, lenDictA)



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



#write the overlap check in C
cdef int overlap_test_c(int qlen, int slen, int qAggrMatch, int sAggrMatch, int qLocMatch, int sLocMatch, float coverageCutoff, float overlapCutoff):
  '''Check overlap (Cython)'''
  # 1: overlap present (the alignment should be skipped)
  # 0: no overlap (the alignment can be used)
  if qAggrMatch < overlapCutoff * qlen:
      return 1
  if qLocMatch < coverageCutoff * qlen:
      return 1
  if sAggrMatch < overlapCutoff * slen:
      return 1
  if sLocMatch < coverageCutoff * slen:
      return 1
  return 0
