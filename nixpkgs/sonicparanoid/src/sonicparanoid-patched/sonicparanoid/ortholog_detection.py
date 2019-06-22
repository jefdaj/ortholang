"""This module contains functions for the detection of orthologs using software like inparanoid."""
import sys
#### IMPORT TO GENERATE PyPi package
#'''
from sonicparanoid import sys_tools as systools
from sonicparanoid import seq_tools as seqtools
# import the module for orthology prediction
from sonicparanoid import inpyranoid
from sonicparanoid import workers
#'''
####

#### IMPORTS TO RUN LOCALLY
'''
import sys_tools as systools
import seq_tools as seqtools
#import the module for orthology prediction
import inpyranoid
import workers
#'''
####
import os
import time
import subprocess
import itertools
from collections import OrderedDict
from collections import deque
import shutil
import multiprocessing as mp
import numpy as np
#this is needed to calculate the dbsize for blastp
#import reads_stats as rstats


__module_name__ = 'Ortholog detection'
__source__ = 'ortholog_detection.py'
__author__ = 'Salvatore Cosentino'
#__copyright__ = ''
__license__ = 'GPL'
__version__ = '2.0'
__maintainer__ = 'Cosentino Salvatore'
__email__ = 'salvo981@gmail.com'

#root path for files
pySrcDir = os.path.dirname(os.path.abspath(__file__))
pySrcDir += '/'
blast_parser = '%sparser.py'%pySrcDir
pckg_root = '%spackages/'%pySrcDir
multiparanoidSrcDir = '%squick_multi_paranoid/'%pySrcDir
inparanoidPckg = '%sinparanoid_mod_cmd_params.tar.gz'%pckg_root
mmseqs2ParserPckg = '%smmseqs2_parser_cython.tar.gz'%pckg_root
quickparanoidPckgRedhat = '%squickparanoid_redhat.tar.gz'%pckg_root
quickparanoidPckgOsx = '%squickparanoid_osx.tar.gz'%pckg_root
# Config file paths
cfgPath = os.path.join(pySrcDir, 'config.json')
# directory in which the mmseqs binaries are located
mmseqsPath = 'mmseqs'



def info():
    """This module contains functions for the detection of orthologs."""
    print('MODULE NAME:\t%s'%__module_name__)
    print('SOURCE FILE NAME:\t%s'%__source__)
    print('MODULE VERSION:\t%s'%__version__)
    print('LICENSE:\t%s'%__license__)
    print('AUTHOR:\t%s'%__author__)
    print('EMAIL:\t%s'%__email__)



def blast_2pass(querySeq, dbSeq, querySeqDict, dbSeqDict, scoreCoff=40, outName=None, outDir=os.getcwd(), threads=4, memMode=False, debug=False):
    """Execute the 2-pass blast implemented in core-inparanoid."""
    if debug:
        print('blast_2pass :: START')
        print('Query:\t%s'%querySeq)
        print('DB Sequences:\t%s'%dbSeq)
        print('Input Sequences dictionary:\t%d'%len(querySeqDict))
        print('DB Sequences dictionary:\t%d'%len(dbSeqDict))
        print('Score cutoff:\t%d'%scoreCoff)
        print('Output name:\t%s'%outName)
        print('Output dir:\t%s'%outDir)
        print('Threads:\t%d'%threads)
        print('Memory mode:\t%s'%memMode)

    #start the timing
    start_time = time.time()
    #check the existence of the input files
    if not os.path.isfile(querySeq):
        sys.stderr.write('The file %s was not found, please provide a input path'%querySeq)
        sys.exit(-2)
    if not os.path.isfile(dbSeq):
        sys.stderr.write('The file %s was not found, please provide a input path'%dbSeq)
        sys.exit(-2)
    #create the output directory if does not exist yet
    if outDir[-1] != '/':
        outDir += '/'
    #create output directory
    systools.makedir(outDir)
    #set the output name
    outputName = ''
    if outName is not None:
        if type(outName) is str:
            outputName = outName.strip()
            outputName = outputName.replace(' ', '') #remove intenal spaces if any
    else: #we might remove the extension as well
        outName = '%s-%s'%(os.path.basename(querySeq), os.path.basename(dbSeq))
    #start the timing
    blast1_start = time.time()
    #Create the blast database
    #EXAMPLE: makeblastdb -in <dbSeq> -dbtype prot
    makeDbCmd = 'makeblastdb -in %s -dbtype prot -out %s%s -title %s'%(dbSeq, outDir, os.path.basename(dbSeq), os.path.basename(dbSeq))
    process = subprocess.Popen(makeDbCmd, shell=True, stdout=subprocess.PIPE, stderr=subprocess.PIPE)
    stdout_val, stderr_val = process.communicate() #get stdout and stderr
    process.wait()
    if debug:
        print('\nMakeblastdb STDOUT:\n%s'%str(stdout_val.decode()))
        print('\nMakeblastdb STDERR:\n%s'%str(stderr_val.decode()))
    #EXAMPLE: blastp -num_threads $threads -seg yes -query $Fld[0] -db $Fld[1] -num_alignments $Fld[3] -matrix $matrix -dbsize 5000000 -outfmt 5 | $blastParser $score_cutoff|"
    numAlign = len(dbSeqDict) #the number of sequences in the database file
    #dbsize = 5000000
    #dbsize = 100000
    dbsize = int(rstats.calcSeqStats(dbSeq, debug=debug)[-1])
    bparser = '%sblast_parser.pl'%outDir
    matrix = 'BLOSUM62'
    prevDir = os.getcwd()
    os.chdir(outDir)
    blastDbPath = '%s%s'%(outDir, os.path.basename(dbSeq))
    ###### STREAMING THE OUTPUT DIRECTLY TO A DICTIONARY  ######
    #execute blast and parsing commands
    hitsDict = OrderedDict() #will contain the parsed blast output
    #blastpCmd = 'blastp -num_threads %d -seg yes -query %s -db %s -num_alignments %d -matrix %s -dbsize %d -outfmt 5'%(threads, querySeq, blastDbPath, numAlign, matrix, dbsize)
    #blastpCmd = 'blastp -num_threads %d -seg yes -query %s -db %s -num_alignments %d -matrix %s -outfmt 5'%(threads, querySeq, blastDbPath, numAlign, matrix)
    #use dbsize
    #blastpCmd = 'blastp -num_threads %d -seg yes -query %s -db %s -num_alignments %d -matrix %s -dbsize %d -outfmt 5'%(threads, querySeq, blastDbPath, numAlign, matrix, dbsize)
    #remove the num_alignments parameter
    blastpCmd = 'blastp -num_threads %d -seg yes -query %s -db %s -dbsize %d -outfmt 5'%(threads, querySeq, blastDbPath, dbsize)
    #evalue 1e-9 num_alignment
    #blastpCmd = 'blastp -num_threads %d -seg yes -query %s -db %s -evalue 0 -num_alignments %d -dbsize %d -outfmt 5'%(threads, querySeq, blastDbPath, numAlign, dbsize)
    if debug:
        print('Blast pass1 CMD for %s-%s'%(os.path.basename(querySeq), os.path.basename(dbSeq)))
        print(blastpCmd)
    #create dictionaries for query and db sequences which are actually required
    #these will be smaller than the complete query and db sequence files
    qDict2Seq = OrderedDict()
    dbDict2Seq = OrderedDict()
    #prepare parse command
    parseBlastCmd = '%s %d'%(bparser, scoreCoff)
    #execute the subprocesses
    blastpProc = subprocess.Popen(blastpCmd, shell=True, stdout=subprocess.PIPE, stderr=subprocess.PIPE)
    #parseBlastProc = subprocess.Popen(parseBlastCmd, shell=True, stdin=blastpProc.stdout, stdout=subprocess.PIPE, stderr=subprocess.PIPE)
    with subprocess.Popen(parseBlastCmd, shell=True, stdin=blastpProc.stdout, stdout=subprocess.PIPE, stderr=subprocess.PIPE, universal_newlines=True) as parseBlastProc:
        for line in parseBlastProc.stdout:
            qHit, dbHit, d1 = line.split('\t', 2)
            if not qHit in hitsDict:
                hitsDict[qHit] = [dbHit]
                #add the query sequence to the smaller dictionaries
                if not qHit in qDict2Seq:
                    qDict2Seq[qHit] = querySeqDict[qHit]
                #print(qDict2Seq)
                #add the db sequence to the smaller dictionary
                if not dbHit in dbDict2Seq:
                    dbDict2Seq[dbHit] = dbSeqDict[dbHit]
            else:
                #hitsDict[qHit] = '%s %s'%(hitsDict[qHit], dbHit)
                hitsDict[qHit].append(dbHit)
                #print('WARNING: double entry attempt for %s'%qHit)
                if not dbHit in dbDict2Seq:
                    dbDict2Seq[dbHit] = dbSeqDict[dbHit]
                #sys.exit('DEBUG :: blast_2pass')
    blastpProc.stdout.close()
    blastpProc.wait()
    blast1_tot = round(time.time() - blast1_start, 2)

    if debug:
        sys.stdout.write('\nBlast_pass1 Elapsed time (seconds):\t%s\n'%str(blast1_tot))
    blast2_start = time.time()

    #remove original dictionaries with sequences
    del querySeqDict, dbSeqDict

    #### PARALLEL EXECUTION ####
    if threads > 1:
        #smaller dictionaries
        single_query_blast_parallel(hitsDict, qDict2Seq, dbDict2Seq, scoreCoff=scoreCoff, dbsize=dbsize, outName=outName, outDir=outDir, threads=threads, outSuffix='chunk', memMode=memMode, debug=debug)
    else:
        #faster mode with smaller dictionaries
        single_query_blast(hitsDict, qDict2Seq, dbDict2Seq, scoreCoff=scoreCoff, dbsize=dbsize, outName=outName, outDir=outDir, debug=debug)
    blast2_tot = round(time.time() - blast2_start, 2)
    sys.stdout.write('\nBlast_pass2 elapsed time (seconds):\t%s\n'%str(blast2_tot))
    tot_time = round(time.time() - start_time, 2)
    sys.stdout.write('\nTotal elapsed time (seconds):\t%s\n'%str(tot_time))
    #merge the results
    outPath = '%s%s'%(outDir, outName)
    if threads > 1:
        ofd = open(outPath, 'w')
        flist = os.listdir(outDir)
        for f in flist:
            if f.startswith('%s_chunk'%outName):
                tmpPath = '%s%s'%(outDir, f)
                if debug:
                    print('Writing %s to output file...'%tmpPath)
                ofd.write(open(tmpPath).read())
                os.remove(tmpPath)
                if debug:
                    print('Removing %s...'%tmpPath)
        ofd.close()
    os.chdir(prevDir)
    return (outPath, blast1_tot, blast2_tot, tot_time)



def blastall_2pass(querySeq, dbSeq, querySeqDict, dbSeqDict, scoreCoff=40, outName=None, outDir=os.getcwd(), threads=4, memMode=False, debug=False):
    """Execute the 2-pass blast implemented in core-inparanoid."""
    if debug:
        print('blastall_2pass :: START')
        print('Query:\t%s'%querySeq)
        print('DB Sequences:\t%s'%dbSeq)
        print('Input Sequences dictionary:\t%d'%len(querySeqDict))
        print('DB Sequences dictionary:\t%d'%len(dbSeqDict))
        print('Score cutoff:\t%d'%scoreCoff)
        print('Output name:\t%s'%outName)
        print('Output dir:\t%s'%outDir)
        print('Threads:\t%d'%threads)
        print('Memory mode:\t%s'%memMode)
    #start the timing
    start_time = time.time()
    #check the existence of the input files
    if not os.path.isfile(querySeq):
        sys.stderr.write('The file %s was not found, please provide a input path'%querySeq)
        sys.exit(-2)
    if not os.path.isfile(dbSeq):
        sys.stderr.write('The file %s was not found, please provide a input path'%dbSeq)
        sys.exit(-2)
    #create the output directory if does not exist yet
    if outDir[-1] != '/':
        outDir += '/'
    #create output directory
    systools.makedir(outDir)
    #set the output name
    outputName = ''
    if outName is not None:
        if type(outName) is str:
            outputName = outName.strip()
            outputName = outputName.replace(' ', '') #remove intenal spaces if any
    else: #we might remove the extension as well
        outName = '%s-%s'%(os.path.basename(querySeq), os.path.basename(dbSeq))
    #start the timing
    blast1_start = time.time()
    #prepare and execute the first blast
    import subprocess
    #Create the blast database
    #EXAMPLE: formatdb -i <dbSeq>
    makeDbCmd = 'formatdb -i %s'%(dbSeq)
    if debug:
        print('formatdb CMD:\t%s'%makeDbCmd)
    process = subprocess.Popen(makeDbCmd, shell=True, stdout=subprocess.PIPE, stderr=subprocess.PIPE)
    stdout_val, stderr_val = process.communicate() #get stdout and stderr
    process.wait()
    if debug:
        print('\nFormatdb STDOUT:\n%s'%str(stdout_val.decode()))
        print('\nFormatdb STDERR:\n%s'%str(stderr_val.decode()))
    #EXAMPLE: blastp -num_threads $threads -seg yes -query $Fld[0] -db $Fld[1] -num_alignments $Fld[3] -matrix $matrix -dbsize 5000000 -outfmt 5 | $blastParser $score_cutoff|"
    numAlign = len(dbSeqDict) #the number of sequences in the database file
    dbsize = 5000000
    bparser = '%sblast_parser.pl'%outDir
    matrix = 'BLOSUM62'
    prevDir = os.getcwd()
    os.chdir(outDir)
    blastDbPath = '%s%s'%(outDir, os.path.basename(dbSeq))
    ###### STREAMING THE OUTPUT DIRECTLY TO A DICTIONARY  ######
    #execute blast and parsing commands
    hitsDict = OrderedDict() #will contain the parsed blast output
    blastpCmd = 'blastall -C3  -F\"m S\" -a %d -i %s -d %s -p blastp -v %d -b %d -M %s -z %d -m 7'%(threads, querySeq, blastDbPath, numAlign, numAlign, matrix, dbsize)
    #open FHR, "$blastall -C3 -F\"m S\" -i $Fld[0] -d $Fld[1] -p blastp -v $Fld[3] -b $Fld[3] -M $matrix -z 5000000 -m7 | ./$blastParser $score_cutoff|";
    if debug:
        print('Blast pass1 CMD for %s-%s'%(os.path.basename(querySeq), os.path.basename(dbSeq)))
        print(blastpCmd)
    parseBlastCmd = '%s %d'%(bparser, scoreCoff)
    #execute the subprocesses
    blastpProc = subprocess.Popen(blastpCmd, shell=True, stdout=subprocess.PIPE, stderr=subprocess.PIPE)
    #parseBlastProc = subprocess.Popen(parseBlastCmd, shell=True, stdin=blastpProc.stdout, stdout=subprocess.PIPE, stderr=subprocess.PIPE)
    with subprocess.Popen(parseBlastCmd, shell=True, stdin=blastpProc.stdout, stdout=subprocess.PIPE, stderr=subprocess.PIPE, universal_newlines=True) as parseBlastProc:
        for line in parseBlastProc.stdout:
            qHit, dbHit, d1 = line.split('\t', 2)
            if not qHit in hitsDict:
                #note that this is different from the original inparanoid
                hitsDict[qHit] = [dbHit]
            else:
                hitsDict[qHit].append(dbHit)
                #print('WARNING: double entry attempt for %s'%qHit)
    blastpProc.stdout.close()
    blastpProc.wait()
    blast1_tot = round(time.time() - blast1_start, 2)
    if debug:
        sys.stdout.write('\nBlast_pass1 Elapsed time (seconds):\t%s\n'%str(blast1_tot))
    blast2_start = time.time()
    #### PARALLEL EXECUTION ####
    single_query_blastall_parallel(hitsDict, querySeqDict, dbSeqDict, scoreCoff=scoreCoff, outName=outName, outDir=outDir, threads=threads, outSuffix='chunk', memMode=memMode, debug=debug)
    blast2_tot = round(time.time() - blast2_start, 2)
    sys.stdout.write('\nBlast_pass2 elapsed time (seconds):\t%s\n'%str(blast2_tot))
    tot_time = round(time.time() - start_time, 2)
    sys.stdout.write('\nTotal elapsed time (seconds):\t%s\n'%str(tot_time))
    #merge the results
    outPath = '%s%s'%(outDir, outName)
    ofd = open(outPath, 'w')
    flist = os.listdir(outDir)
    for f in flist:
        if f.startswith('%s_chunk'%outName):
            tmpPath = '%s%s'%(outDir, f)
            if debug:
                print('Writing %s to output file...'%tmpPath)
            ofd.write(open(tmpPath).read())
            os.remove(tmpPath)
            if debug:
                print('Removing %s...'%tmpPath)
    ofd.close()
    os.chdir(prevDir)
    return (outPath, blast1_tot, blast2_tot, tot_time)



def calc_inparanoid_exec_time(inDir, outDir=os.getcwd(), debug=True):
    """Calculate the execution times for inparanoid."""
    if debug:
        print('calc_inparanoid_exec_time :: START')
        print('Input dir:%s'%inDir)
        print('Outdir:%s'%outDir)
    #check the existence of the input dir
    if not os.path.isdir(inDir):
        sys.stderr.write('ERROR: The directory %s was not found, please provide a path to a valid directory.\n'%inDir)
        sys.exit(-2)
    #create the output directory if does not exist yet
    if outDir != os.getcwd():
        if not os.path.isdir(outDir):
            systools.makedir(outDir)
    if outDir[-1] != '/':
        outDir += '/'
    #check the ecistance of the main output file and the get the input names
    flist = os.listdir(inDir)
    outFilePath = None
    for fname in flist:
        if fname.startswith('table.'):
            outFilePath = '%s%s'%(inDir, fname.strip())
            break
    #make sure the output file was found
    if outFilePath is None:
        sys.stderr.write('ERROR: No final output file was found for the current directory, please run inparanoid first.\n')
        sys.exit(-5)
    #extract the input file names
    bname = os.path.basename(outFilePath).replace('table.', '')
    #check now that the start timestamp is present
    if not os.path.isfile('%ss_master'%(inDir)):
        sys.stderr.write('ERROR: the \'s_master\' file with the start timestamp was not found in the current directory, please run inparanoid first.\n')
        return None
    #check that the final timestamp exists
    if not os.path.isfile('%se_run_inparanoid'%(inDir)):
        sys.stderr.write('ERROR: the \'e_run_inparanoid\' file with the final timestamp was not found in the current directory, please run inparanoid first.\n')
        return None
    fname1, fname2 = bname.split('-')
    aa_map = '%s%s'%(fname1, fname1)
    print(aa_map)
    #sys.exit('DEBUG')
    s_blastAA2pass_ts = None
    bb_map = '%s%s'%(fname2, fname2)
    s_blastBB2pass_ts = None
    ab_map = '%s%s'%(fname1, fname2)
    s_blastAB2pass_ts = None
    ba_map = '%s%s'%(fname2, fname1)
    s_blastBA2pass_ts = None
    #now search the files with timestamps for the 2nd pass blast
    for fname in flist:
        if fname.endswith(aa_map):
            s_blastAA2pass_ts = '%s%s'%(inDir, fname.strip())
        elif fname.endswith(bb_map):
            s_blastBB2pass_ts = '%s%s'%(inDir, fname.strip())
        elif fname.endswith(ab_map):
            s_blastAB2pass_ts = '%s%s'%(inDir, fname.strip())
        elif fname.endswith(ba_map):
            s_blastBA2pass_ts = '%s%s'%(inDir, fname.strip())
    #now start calculating the execution time
    start_ts = '%ss_master'%(inDir)
    end_ts = '%se_run_inparanoid'%(inDir)
    tot_ex_time = systools.getElapsedTime(start_ts, end_ts, timeStamps=True, debug=debug)[0]
    #calculate time for blast AA
    s_blastAA_ts = '%ss_do_blast_aa'%(inDir)
    e_blastAA_ts = '%se_do_blast_aa'%(inDir)
    tot_blastAA = systools.getElapsedTime(s_blastAA_ts, e_blastAA_ts, timeStamps=True, debug=debug)[0]
    pass2_blastAA = systools.getElapsedTime(s_blastAA2pass_ts, e_blastAA_ts, timeStamps=True, debug=debug)[0]
    #calculate time for blast BB
    s_blastBB_ts = '%ss_do_blast_bb'%(inDir)
    e_blastBB_ts = '%se_do_blast_bb'%(inDir)
    tot_blastBB = systools.getElapsedTime(s_blastBB_ts, e_blastBB_ts, timeStamps=True, debug=debug)[0]
    pass2_blastBB = systools.getElapsedTime(s_blastBB2pass_ts, e_blastBB_ts, timeStamps=True, debug=debug)[0]
    #sys.exit('DEBUG')
    #calculate time for blast AB
    s_blastAB_ts = '%ss_do_blast_ab'%(inDir)
    e_blastAB_ts = '%se_do_blast_ab'%(inDir)
    tot_blastAB = systools.getElapsedTime(s_blastAB_ts, e_blastAB_ts, timeStamps=True, debug=debug)[0]
    pass2_blastAB = systools.getElapsedTime(s_blastAB2pass_ts, e_blastAB_ts, timeStamps=True, debug=debug)[0]
    #calculate time for blast BA
    s_blastBA_ts = '%ss_do_blast_ba'%(inDir)
    e_blastBA_ts = '%se_do_blast_ba'%(inDir)
    tot_blastBA = systools.getElapsedTime(s_blastBA_ts, e_blastBA_ts, timeStamps=True, debug=debug)[0]
    pass2_blastBA = systools.getElapsedTime(s_blastBA2pass_ts, e_blastBA_ts, timeStamps=True, debug=debug)[0]
    #calculate time for runInparanoid
    s_runinparanoid_ts = '%ss_run_inparanoid'%(inDir)
    tot_ortholog_calc_ts = systools.getElapsedTime(s_runinparanoid_ts, end_ts, timeStamps=True, debug=debug)[0]
    #open the output file
    outFile = '%sexecution.%s-%s'%(outDir, fname1, fname2)

    #write the file with execution time
    executionTime = 'execution_time.%s-%s'%(fname1, fname2)
    executionTimePath = '%s%s'%(outDir, executionTime)
    #count sequences in first input file
    f1SeqCnt = f2SeqCnt = 0
    ifd = open('%s%s'%(inDir, fname1))
    for ln in ifd:
        if ln.strip() == '': #empty line
            continue
        if ln[0] == '>':
            f1SeqCnt += 1
    ifd.close()
    #count sequences for second file
    ifd = open('%s%s'%(inDir, fname2))
    for ln in ifd:
        if ln.strip() == '': #empty line
            continue
        if ln[0] == '>':
            f2SeqCnt += 1
    ifd.close()
    #open and write the output file
    ofd = open(executionTimePath, 'w')
    ofd.write('input1\tinput2\tinput1_size\tinput1_size\tblastAA_pass1\tblastAA_pass2\tblastAA_tot\tblastAB_pass1\tblastAB_pass2\tblastAB_tot\tblastBA_pass1\tblastBA_pass2\tblastBA_tot\tblastBB_pass1\tblastBB_pass2\tblastBB_tot\tinparanoid\ttotal_runtime\n')
    #now write the values
    #count the input sequences
    ofd.write('%s\t%s\t%d\t%d\t%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s\n'%(fname1, fname2, f1SeqCnt, f2SeqCnt, tot_blastAA - pass2_blastAA, pass2_blastAA, tot_blastAA, tot_blastAB - pass2_blastAB, pass2_blastAB, tot_blastAB, tot_blastBA - pass2_blastBA, pass2_blastBA, tot_blastBA, tot_blastBB - pass2_blastBB, pass2_blastBB, tot_blastBB, tot_ortholog_calc_ts, tot_ex_time))
    ofd.close()
    #return the output path and total time
    return  (tot_ex_time, executionTimePath)



def calc_ortholog_group_stats(rootDir=os.getcwd(), outDir=os.getcwd(), outName=None, pairsFile=None, debug=False):
    """Calculate simple stats on all the generated ortholog groups, based on the group bitscore."""
    if debug:
        print('calc_ortholog_group_stats :: START')
        print('Root directory:\t%s'%rootDir)
        print('Output directory:\t%s'%outDir)
        print('Output file name:\t%s'%outName)
        print('Species pairs file:\t%s'%pairsFile)
    #fetch result files tables
    tblList = fetch_inparanoid_tables(rootDir=rootDir, outDir=outDir, pairsFile=pairsFile, tblPrefix='table', debug=debug)
    cumulativeScores = [] #contains all the bitscores
    tblSizes = [] #contain all the table sizes
    #create the output directory if it does not exist
    systools.makedir(outDir)
    #set the output path
    #extract the project name from the root
    projName = ''
    if rootDir[-1] == '/':
        projName = rootDir.rsplit('/', 2)[-2]
    else:
        projName = rootDir.rsplit('/', 2)[-1]
    if outName is None:
        outName = '%s_relations_stats.dat'%projName
    #output file
    outTbl = '%s%s'%(outDir, outName)
    tblTmpVals = None
    tblCnt = 0
    for path in tblList:
        if os.path.isfile(path):
            tblTmpVals = []
            tblCnt += 1
            for ln in open(path):
                if ln[0] == 'O': #the column names
                    continue
                #totRead += 1
                ln = ln.rstrip()
                #print(path)
                #print(ln)
                d1, score, d2 = ln.split('\t', 2)
                score = int(score)
                cumulativeScores.append(score)
                tblTmpVals.append(score) #values for the single table
        #summarize the counts
        tblTmpVals = np.array(tblTmpVals)
        tblSizes.append(len(tblTmpVals))
    #calculate final stats
    #create output file
    tblSizes = np.array(tblSizes)
    ofd = open(outTbl, 'w')
    ofd.write('Total tables read:\t%d\n'%len(tblSizes))
    ofd.write('Total inparanoid table entries read:\t%d\n'%int(np.sum(tblSizes)))
    ofd.write('\nTable sizes\n')
    ofd.write('Max:\t%s\n'%str(int(np.max(tblSizes))))
    ofd.write('Min:\t%s\n'%str(int(np.min(tblSizes))))
    ofd.write('Avg:\t%s\n'%str(round(float(np.mean(tblSizes)), 2)))
    ofd.write('Std:\t%s\n'%str(round(float(np.std(tblSizes)), 2)))
    ofd.write('\nCumulative scores\n')
    del tblSizes
    cumulativeScores = np.array(cumulativeScores)
    ofd.write('Max:\t%s\n'%str(int(np.max(cumulativeScores))))
    ofd.write('Min:\t%s\n'%str(int(np.min(cumulativeScores))))
    ofd.write('Avg:\t%s\n'%str(round(float(np.mean(cumulativeScores)), 2)))
    ofd.write('Std:\t%s\n'%str(round(float(np.std(cumulativeScores)), 2)))
    ofd.close()



def check_hsp_overlap(hsp1, hsp2, debug=True):
    """Check if there is overlap among the 2 hsp."""
    if debug:
        print('\ncheck_hsp_overlap :: START')
        print('hsp1:\t%s'%str(hsp1))
        print('hsp2:\t%s'%str(hsp2))
    #print('Current HSP:\t%d\t%d'%(qstart, qend))
    if hsp1[0] == hsp2[0]: #then there is an overlap for sure
        #print('Overlap found SAME START!')
        #print('hsp1:\t%s'%str(hsp1))
        #print('hsp2:\t%s'%str(hsp2))
        return True
    #position the 2 hsp first
    lxHsp = dxHsp = None
    if hsp1[0] < hsp2[0]:
        lxHsp = (hsp1[0], hsp1[1])
        dxHsp = (hsp2[0], hsp2[1])
    else: #tmpS < qstart
        lxHsp = (hsp2[0], hsp2[1])
        dxHsp = (hsp1[0], hsp1[1])
    lenLxHsp = lxHsp[1] - lxHsp[0] + 1
    lenDxHsp = dxHsp[1] - dxHsp[0] + 1
    #find the shortest HSP
    shortestHsp = min(lenLxHsp, lenDxHsp)
    #calculate the overlap
    overlap = round((dxHsp[0] - lxHsp[1] - 1.) / float(shortestHsp), 4)
    #print('Overlap score:\t%s\n'%str(overlap))
    if overlap <= -0.05:
        #print('Overlap found!')
        return True
    return False



def count_clusters(inTbl, debug=False):
    """Count the clusters with orthologs."""
    import pandas as pd
    df = pd.read_csv(inTbl, sep='\t')
    dfuniq = df.drop_duplicates(subset=[df.columns[0], 'tree_conflict'], keep='first')
    if debug:
        print(dfuniq.describe())
    #print(dfuniq.describe(percentiles=None))
    no_conflict = len(dfuniq.query('tree_conflict == \"No\"'))
    diff_names = len(dfuniq.query('tree_conflict == \"diff. names\"'))
    diff_numbers = len(dfuniq.query('tree_conflict == \"diff. numbers\"'))
    tot_conflicts = diff_names + diff_numbers
    clstrCnt = len(dfuniq)
    if debug:
        print('Clusters:\t%d'%clstrCnt)
        print('No conflict:\t%d'%no_conflict)
        print('diff.names conflict:\t%d'%diff_names)
        print('diff.numbers conflict:\t%d'%diff_numbers)
        print('Total conflicts:\t%d'%tot_conflicts)
    #return the main numbers
    return(clstrCnt, no_conflict, diff_names, diff_numbers)



def count_clusters_no_pandas(inTbl, debug=False):
    """Count the clusters with orthologs."""
    from collections import Counter
    tmpDict = {} #will contain the ids
    cnt = Counter()
    #open and read the table file
    for line in open(inTbl):
        if line.startswith('#cl') or line.startswith('clusterID'):
            continue
        line = line.rstrip()
        flds = line.split('\t')
        clstrId = flds[0]
        if not clstrId in tmpDict:
            tmpDict[clstrId] = None
            cnt.update([flds[-1]]) #count the type of conflict
    no_conflict = cnt['No']
    diff_names = cnt['diff. names']
    diff_numbers = cnt['diff. numbers']
    tot_conflicts = diff_names + diff_numbers
    clstrCnt = tot_conflicts + no_conflict
    if debug:
        print('Clusters:\t%d'%clstrCnt)
        print('No conflict:\t%d'%no_conflict)
        print('diff.names conflict:\t%d'%diff_names)
        print('diff.numbers conflict:\t%d'%diff_numbers)
        print('Total conflicts:\t%d'%tot_conflicts)
    #return the main numbers
    return(clstrCnt, no_conflict, diff_names, diff_numbers)



def copy_quickparanoid_files(srcDir, outDir=os.getcwd(), debug=False):
    """Copy the source and binary files for quickparanoid in the output directory."""
    if debug:
        print('copy_quickparanoid_files :: START')
        print('Source directory:\t%s'%srcDir)
        print('QuickParanoid output directory:\t%s'%outDir)
    if os.path.realpath(srcDir) == os.path.realpath(outDir):
        if debug:
            sys.stderr.write('\nINFO: output and source directory are same, no file will be copied.\n')
    else: # copy the files
        # traverse the directory
        for dirPath, dirNames, fNames in os.walk(srcDir):
            # create the output directory if required
            systools.makedir(outDir)
            # copy files
            for f in fNames:
                tmpPath = os.path.join(srcDir, f)
                systools.copy(tmpPath, outDir)



def check_2species_run(rootDir, sp1, sp2, sharedDir, debug=False):
    """Check the integrity of a 2-species ortholog search."""
    if debug:
        print('check_2species_run :: START')
        print('Root directory:\t%s'%rootDir)
        print('Species A:\t%s'%sp1)
        print('Species B:\t%s'%sp2)
        print('Shared directory:\t%s'%sharedDir)
    #path to run doirectory
    pair = '%s-%s'%(sp1, sp2)
    runDir = '%s%s/'%(rootDir, pair)
    if not os.path.isdir(runDir):
        sys.stderr.write('ERROR: the directory containing the run\n%s\n does not exist.\n'%runDir)
        sys.exit(-2)
    #output variables
    runOk = False
    missingComp = []
    #calculate all the possible combinations
    rawComb = itertools.combinations_with_replacement([sp1, sp2], 2)
    comparisonPaths = []
    for tpl in rawComb:
        comparisonPaths.append('%s%s-%s'%(runDir, tpl[0], tpl[-1]))
    # add the pair BA
    comparisonPaths.append('%s%s-%s'%(runDir, sp2, sp1))
    #check if the files exists
    for path in comparisonPaths:
        if os.path.isfile(path):
            s1, s2 = os.path.basename(path).split('-')
            if s1 == s2: #check if it is a within species
                sharedFile = '%s%s-%s'%(sharedDir, s1, s2)
                #sys.exit('debug')
                if not os.path.isfile(sharedFile):
                    systools.copy(path, sharedDir, debug=debug)
        else:
            missingComp.append(os.path.basename(path))
    #check if the sql directory table exists
    tblFile = '%stable.%s'%(runDir, pair)
    sqlFile = '%ssqltable.%s'%(runDir, pair)
    #outFile = '%sOutput.%s'%(runDir, pair)
    #if (os.path.isfile(tblFile)) and (os.path.isfile(sqlFile)) and (os.path.isfile(outFile)):
    if (os.path.isfile(tblFile)) and (os.path.isfile(sqlFile)):
        runOk = True
    if debug:
        print('Missing comparisons:\n')
        print(missingComp)
    #sys.exit('DEBUG')
    return(runOk, missingComp)



def extract_inparanoid_scores(hspDict, qid, sid, debug=False):
    """Extract the scores for an inparanoid graph-node.

    Also the overlaps are calculated
    Return a tab-separated similar to the following:
    63363_O67184 63363_O66911 75.5 564 926 214 303 133 136 q:324-380 h:578-634	q:462-537 h:802-880
    """
    if debug:
        print('\nextract_inparanoid_scores :: START')
        print('Query:\t%s'%qid)
        print('Subject:\t%s'%sid)
        print('Number of HSP:\t%d'%len(hspDict))
    # the meaning of the fileds are the following
    #col1: query
    #col2: subject
    #col3: sum( hsp_i.bitscore )
    #col4: query length [qlen]
    #col5: subject length [slen]
    #col6: max([hsp_1.qend, hsp_2.qend, ... hsp_N.qend]) - min([hsp_1.qstart, hsp_2.qstart, ... hsp_N.qstart] + 1)
    #col7: max([hsp_1.send, hsp_2.send, ... hsp_N.send]) - min([hsp_1.sstart, hsp_2.sstart, ... hsp_N.sstart] + 1)
    #col8: for i=[1, N], sum([hsp_i.qend - hsp_i.qstart] + 1)
    #col9: for i=[1, N], sum([hsp_i.send - hsp_i.sstart] + 1)
    #col10: tab-separated list of all hsp start and end of query subject, in ascending order of the qstart value
    # example of value in col10: q:324-380 h:578-634	q:462-537 h:802-880
    #each entry in the dictionary has qstart for the hsp as key
    #and the following information as values: qlen, slen, qstart, qend, sstart, send, bitscore
    #calculate score in the simple case in which there is only one HSP
    qFragmentList = [] #contain tuples with start and end positions of hsp on query
    hFragmentList = [] #contain tuples with start and end positions of hsp on hit
    tmpDict = {} # will have the qstart as keys and strings like (q:324-380 h:578-634	q:462-537 h:802-880) as values
    if len(hspDict) == 1:
        qlen, slen, qstart, qend, sstart, send, bitscore = list(hspDict.values())[0]
        #return(qid, sid, str(bitscore), str(qlen), str(slen), str(qend - qstart + 1), str(send - sstart + 1), str(qend - qstart + 1), str(send - sstart + 1), ['q:%d-%d h:%d-%d'%(qstart, qend, sstart, send)])
        return(qid, sid, str(bitscore), str(qlen), str(slen), str(qend - qstart + 1), str(send - sstart + 1), str(qend - qstart + 1), str(send - sstart + 1), ['q:%d-%d h:%d-%d'%(qstart, qend, sstart, send)])
    else:
        fBitscore = 0.
        fQlen = fSlen = fCol6 = fCol7 = fCol8 = fCol9 = 0
        #these will be used to calculate the overlap
        #and represent the: rightmost end of hsp on query, rightmost end on hsp on hit, leftmost start on hsp on query,  leftmost start on hsp on hit
        dxQuery = dxHit = lxQuery = lxHit = None
        i = 0
        for hsp in hspDict:
            i += 1
            qlen, slen, qstart, qend, sstart, send, bitscore = list(hspDict[hsp])
            if len(qFragmentList) == 0: #then it is the first hsp and must be included in the count
                qFragmentList.append((qstart, qend))
                hFragmentList.append((sstart, send))
                fQlen = qlen
                fSlen = slen
                dxQuery = qend
                lxQuery = qstart
                dxHit = send
                lxHit = sstart
                fBitscore += bitscore
                tmpDict['%d:%d'%(qstart, qend)] = 'q:%d-%d h:%d-%d'%(qstart, qend, sstart, send)
            else:
                overlapFound = False #used to decide if the current hsp should be included in the final score or not
                #check if there is any overlap on the query
                for interval in qFragmentList:
                    overlapFound = check_hsp_overlap((qstart, qend), interval, debug)
                    if overlapFound:
                        #print('Overlap found:\tQUERY')
                        break
                #check if there is any overlap on the hit
                if not overlapFound:
                    for interval in hFragmentList:
                        overlapFound = check_hsp_overlap((sstart, send), interval, debug)
                        if overlapFound:
                            #print('Overlap found:\tSUBJECT')
                            break
                #if the overlap was found just skip the hsp
                if overlapFound:
                    continue
                #otherwise include the current hsp
                qFragmentList.append((qstart, qend))
                hFragmentList.append((sstart, send))
                fBitscore += bitscore
                tmpDict['%d:%d'%(qstart, qend)] = 'q:%d-%d h:%d-%d'%(qstart, qend, sstart, send)
        #finalize the output record
        fBitscore = round(fBitscore, 2)
        #dictionary for strings in col10
        col10Dict = {}
        #insert values in the col10Dict with the qstart as key value (tmpDict dict should not contain same values)
        for k in tmpDict:
            tmpStart = k.split(':')[0]
            col10Dict[int(tmpStart)] = tmpDict[k]
        del tmpDict
        #sort the values for col10
        sorted_list = sorted(col10Dict.items(), key=lambda x: x[0])
        col10List = []
        for el in sorted_list:
            col10List.append(el[1])
        #calculate values for column 6, 7, 8, 9
        #load required values for hsp in queries
        tmpStart = []
        tmpEnd = []
        #col8: for i=[1, N], sum([hsp_i.qend - hsp_i.qstart] + 1)
        for el in qFragmentList:
            tmpStart.append(el[0])
            tmpEnd.append(el[1])
            fCol8 += el[1] - el[0] + 1
        #col6: max([hsp_1.qend, hsp_2.qend, ... hsp_N.qend]) - min([hsp_1.qstart, hsp_2.qstart, ... hsp_N.qstart]) + 1
        fCol6 = max(tmpEnd) - min(tmpStart) + 1
        #load required values for hsp in subjects
        tmpStart.clear()
        tmpEnd.clear()
        #col9: for i=[1, N], sum([hsp_i.send - hsp_i.sstart] + 1)
        for el in hFragmentList:
            tmpStart.append(el[0])
            tmpEnd.append(el[1])
            fCol9 += el[1] - el[0] + 1
        #col7: max([hsp_1.send, hsp_2.send, ... hsp_N.send]) - min([hsp_1.sstart, hsp_2.sstart, ... hsp_N.sstart] + 1)
        fCol7 = max(tmpEnd) - min(tmpStart) + 1
    #return the required values
    return(qid, sid, str(fBitscore), str(qlen), str(slen), str(fCol6), str(fCol7), str(fCol8), str(fCol9), col10List)



def extract_ortholog_pairs(rootDir=os.getcwd(), outDir=os.getcwd(), outName=None, pairsFile=None, coreOnly=False, splitMode=False, debug=False):
    """Create file containing all generated ortholog pairs."""
    if debug:
        print('extract_ortholog_pairs :: START')
        print('Root directory:\t%s'%rootDir)
        print('Output directory:\t%s'%outDir)
        print('Output file name:\t%s'%outName)
        print('Species pairs file:\t%s'%pairsFile)
        print('Core only:\t%s'%coreOnly)
        # keep only first part of the gene id after splitting on the '_' character (if any)
        print('Split mode:\t%s'%splitMode)
    #fetch result files tables
    tblList = fetch_inparanoid_tables(rootDir=rootDir, outDir=outDir, pairsFile=pairsFile, debug=debug)
    totRead = totWrite = tblCnt = 0
    coreClstrMissCnt = 0
    #extract the project name from the root
    projName = ''
    if rootDir[-1] == '/':
        projName = rootDir.rsplit('/', 2)[-2]
    else:
        projName = rootDir.rsplit('/', 2)[-1]
    if outName is None:
        if coreOnly:
            outName = '%s_core_relations.tsv'%projName
        else:
            outName = '%s_all_relations.tsv'%projName
    #create output directory if required
    systools.makedir(outDir)
    #output file
    outTbl = '%s%s'%(outDir, outName)
    # this dictionary is to avoid repetition among the non-core pairs
    repeatTrap = {}
    print('Creating file with homolog pairs...')
    #create output file
    ofd = open(outTbl, 'w')
    for path in tblList:
        if os.path.isfile(path):
            if debug:
                print(path)
            if os.path.basename(path).startswith('table.'):
                tblCnt += 1
                for clstr in open(path):
                    if clstr[0] == 'O':
                        continue
                    totRead += 1
                    clusterID, score, orto1, orto2 = clstr.rstrip().split('\t')
                    #count the cases
                    ortho1All = orto1.rstrip().split(' ')
                    ortho2All = orto2.rstrip().split(' ')
                    #will associate scores to ortholog genes
                    orthoScoresDict = OrderedDict()
                    for i, gene in enumerate(ortho1All):
                        if i % 2 == 0:
                            if splitMode:
                                orthoScoresDict[gene.split('_', 1)[1]] = round(float(ortho1All[i + 1]), 2)
                            else:
                                orthoScoresDict[gene] = round(float(ortho1All[i + 1]), 2)
                    #now the second part of the cluster...
                    for i, gene in enumerate(ortho2All):
                        if i % 2 == 0:
                            if splitMode:
                                orthoScoresDict[gene.split('_', 1)[1]] = round(float(ortho2All[i + 1]), 2)
                            else:
                                orthoScoresDict[gene] = round(float(ortho2All[i + 1]), 2)
                    #make lists with gene ids
                    ortho1list = []
                    #extract genes for ortho1
                    for i, gene in enumerate(ortho1All):
                        if i % 2 == 0:
                            if splitMode:
                                ortho1list.append(gene.split('_', 1)[1])
                            else:
                                ortho1list.append(gene)
                    ortho1AllLen = len(ortho1list)
                    #extract genes for ortho2
                    ortho2list = []
                    #extract genes for ortho1
                    for i, gene in enumerate(ortho2All):
                        if i % 2 == 0:
                            if splitMode:
                                ortho2list.append(gene.split('_', 1)[1])
                            else:
                                ortho2list.append(gene)
                    ortho2AllLen = len(ortho2list)
                    #write the pairs in the output file
                    if coreOnly: #add only the ortholog relation with 1.0 as score
                        #check the the score is 1.0
                        pairFound = False
                        coreCnt = 0
                        for orto1gene in ortho1list:
                            if orthoScoresDict[orto1gene] == 1.0:
                                for orto2gene in ortho2list:
                                    #count the core relations written
                                    if orthoScoresDict[orto2gene] == 1.0:
                                        ofd.write('%s\t%s\n'%(orto1gene, orto2gene))
                                        totWrite += 1
                                        coreCnt += 1
                                    pairFound = True
                        if not pairFound:
                            if debug:
                                print('WARNING: the CORE pair was not found:\n%s'%clstr)
                            coreClstrMissCnt += 1
                    else: #write all the ortholog relations
                        for orto1gene in ortho1list:
                            for orto2gene in ortho2list:
                                tmpPair = '%s-%s'%(orto1gene, orto2gene)
                                if not tmpPair in repeatTrap:
                                    repeatTrap[tmpPair] = None
                                    ofd.write('%s\t%s\n'%(orto1gene, orto2gene))
                                totWrite += 1
    ofd.close()
    # sort the output file alphabetically
    from sh import sort
    tmpSortPath = os.path.join(outDir, 'tmp_sorted_orthologs.tsv')
    # sort using sh
    print('Sorting homolog pairs...')
    sort(outTbl, '-o', tmpSortPath)
    # remove the original ortholog pairs file
    os.remove(outTbl)
    # rename the sorted file to the original output name
    os.rename(tmpSortPath, outTbl)
    if debug:
        print('Total clusters read:\t%d'%totRead)
        if coreOnly:
            print('Total CORE clusters read:\t%d'%(totRead - coreClstrMissCnt))
    # write the number of ortholog relations in created
    from io import StringIO
    from sh import wc
    buf = StringIO()
    wc('-l', outTbl, _out=buf)
    pairsCnt = buf.getvalue().strip().split(' ', 1)[0]
    print("Total orthologous relations\t{:s}".format(pairsCnt))



def fetch_inparanoid_tables(rootDir=os.getcwd(), outDir=os.getcwd(), pairsFile=None, tblPrefix='table', debug=False):
    """Find result inparanoid table files for each proteome pair."""
    import fnmatch
    if debug:
        print('fetch_inparanoid_tables :: START')
        print('Root directory:\t%s'%rootDir)
        print('Output directory:\t%s'%outDir)
        print('Species pairs file:\t%s'%pairsFile)
        print('Table prefix:\t%s'%tblPrefix)
        # the output table prefix can be 'table' for ortholog tables, or 'Output' for tables with bitscores
    #check that the input directory is valid
    if not os.path.isdir(rootDir):
        sys.stderr.write('ERROR: the directory containing the inparanoid output files\n%s\n does not exist.\n'%rootDir)
        sys.exit(-2)
    if not os.path.isfile(pairsFile):
        sys.stderr.write('ERROR: you must provide a valid file containing all the species pairs\n')
        sys.exit(-2)
    #create the output directory if does not exist yet
    if outDir[-1] != '/':
        outDir += '/'
    systools.makedir(outDir)
    #load the species names
    pairs = OrderedDict()
    foundPairs = OrderedDict()
    species = OrderedDict()
    #enter the root directory
    prevDir = os.getcwd()
    os.chdir(rootDir)
    #find the inparanoid table files
    fileList = []
    for pair in open(pairsFile):
        pair = pair.rstrip('\n')
        pairs[pair] = None
        sp1, sp2 = pair.split('-')
        species[sp1] = None
        species[sp2] = None
        #make the file paths
        runPath = '%s%s/'%(rootDir, pair)
        tblName = '%s.%s'%(tblPrefix, pair)
        if os.path.isdir(runPath):
            tblPath = '%s%s'%(runPath, tblName)
            if os.path.isfile(tblPath):
                fileList.append(tblPath)
                if debug:
                    print(tblPath)
                foundPairs[pair] = None
    #check that the found tables and the species-pairs count are same
    if len(foundPairs) != len(pairs):
        sys.stderr.write('ERROR: the number of ortholog tables found (%d) and the number of species pairs (%d) must be the same.\n'%(len(foundPairs), len(pairs)))
        print('\nMissing ortholog tables for pairs:')
        # check which pair is missing
        tmpList = []
        for p in pairs:
            if p not in foundPairs:
                tmpList.append(p)
        print(' '.join(tmpList))
        sys.exit(-2)
    #reset the current directory to the previous one
    os.chdir(prevDir)
    if debug:
        print('Found tables:\t%d'%len(fileList))
    #return the final list
    return fileList



def fetch_sql_files(rootDir=os.getcwd(), outDir=os.getcwd(), pairsFile=None, coreOnly=False, debug=False):
    """Find result SQL tables and copy it to the output directory."""
    import fnmatch
    if debug:
        print('fetch_sql_paths :: START')
        print('Root directory:\t%s'%rootDir)
        print('Output directory:\t%s'%outDir)
        print('Core only:\t%s'%coreOnly)
        print('Species pairs file:\t%s'%pairsFile)
    #check that the input directory is valid
    if not os.path.isdir(rootDir):
        sys.stderr.write('ERROR: the directory containing the inparanoid output files\n%s\n does not exist.\n'%rootDir)
        sys.exit(-2)
    if not os.path.isfile(pairsFile):
        sys.stderr.write('ERROR: you must provide a valid file containing all the species pairs\n')
        sys.exit(-2)
    #create the output directory if does not exist yet
    if outDir[-1] != '/':
        outDir += '/'
    systools.makedir(outDir)
    #load the species names
    pairs = OrderedDict()
    foundPairs = OrderedDict()
    species = OrderedDict()
    #enter the root directory
    prevDir = os.getcwd()
    os.chdir(rootDir)
    #find the sql files
    fileList = []
    for pair in open(pairsFile):
        pair = pair.rstrip()
        pairs[pair] = None
        sp1, sp2 = pair.split('-')
        species[sp1] = None
        species[sp2] = None
        #make the file paths
        runPath = '%s%s/'%(rootDir, pair)
        sqlName = 'sqltable.%s'%pair
        if os.path.isdir(runPath):
            sqlPath = '%s%s'%(runPath, sqlName)
            if os.path.isfile(sqlPath):
                fileList.append(sqlPath)
                if debug:
                    print(sqlPath)
                foundPairs[pair] = None
    #check that the found tables and the species-pairs count are same
    if len(foundPairs) != len(pairs):
        sys.stderr.write('ERROR: the number found sqltable files (%d) and the number of species pairs (%d) must be the same.\n'%(len(fileList), len(pairs)))
        print('\nMissing sql tables for pairs:')
        # check which pair is missing
        tmpList = []
        for p in pairs:
            if p not in foundPairs:
                tmpList.append(p)
        print(' '.join(tmpList))
        sys.exit(-2)
    for el in fileList:
        systools.copy(el, outDir, metaData=False, debug=debug)
        if coreOnly:
            newPath = '%s%s'%(outDir, os.path.basename(el))
            filter_sql_tbl_core_orthologs(newPath, debug=debug)
            #filter_sql_tbl_by_confidence(newPath, confThr=0.3, debug=debug)
    #reset the current directory to the previous one
    os.chdir(prevDir)
    #return the final list
    return fileList



def filter_sql_tbl_by_confidence(inTbl, confThr=0.3, debug=False):
    """Filter sql table to include only homologs above a given threshols."""
    if debug:
        print('filter_sql_tbl_by_confidence :: START')
        print('Input sql table:\t%s'%inTbl)
        print('Minimum confidence:\t%s'%(str(confThr)))
    if not os.path.isfile(inTbl):
        sys.stderr.write('ERROR: the file with sql table \n%s\n does not exist.\n'%inTbl)
        sys.exit(-5)
    #create tmp file
    outDir = '%s/'%(os.path.dirname(inTbl))
    tmpFile = '%stmp.txt'%(outDir)
    #example of line in sql table
    #1 3993 jcm_1507 1.000 jcm_1507_scaffold_3_gene4130 100%
    ofd = open(tmpFile, 'w')
    wCnt = rCnt = 0
    #start reading the input table
    for ln in open(inTbl):
        rCnt += 1
        ln = ln.rstrip()
        flds = ln.split('\t')
        confidence = float(flds[3])
        if confidence >= confThr: #core ortholog
            ofd.write('%s\n'%ln)
            wCnt += 1
    #rename tmp to the oringinal input file
    systools.move(tmpFile, inTbl, debug=True)
    if debug:
        print('Read entries:\t%d'%rCnt)
        print('Wrote CORE orthologs:\t%d'%wCnt)
    ofd.close()



def filter_sql_tbl_core_orthologs(inTbl, debug=False):
    """Filter sql table to include only CORE orhtologous genes."""
    if debug:
        print('filter_sql_tbl_core_orthologs :: START')
        print('Input sql table:\t%s'%inTbl)
    if not os.path.isfile(inTbl):
        sys.stderr.write('ERROR: the file with sql table \n%s\n does not exist.\n'%inTbl)
        sys.exit(-5)
    #create tmp file
    outDir = '%s/'%(os.path.dirname(inTbl))
    tmpFile = '%stmp.txt'%(outDir)
    #example of line in sql table
    #1 3993 jcm_1507 1.000 jcm_1507_scaffold_3_gene4130 100%
    ofd = open(tmpFile, 'w')
    wCnt = rCnt = 0
    #start reading the input table
    for ln in open(inTbl):
        rCnt += 1
        ln = ln.rstrip()
        flds = ln.split('\t')
        confidence = float(flds[3])
        if confidence == 1: #core ortholog
            ofd.write('%s\n'%ln)
            wCnt += 1
    ofd.close()
    #rename tmp to the oringinal input file
    systools.move(tmpFile, inTbl, debug=debug)
    if debug:
        print('Read entries:\t%d'%rCnt)
        print('Wrote CORE orthologs:\t%d'%wCnt)


# NOTE: CHECK IF THE CONFIG FILE EXISTS, AND PROMPT THE USER ABOUT IT
def get_mmseqs_path():
    """Return the directory in which MMseqs2 binaries are stored."""
    import platform
    #pySrcDir = os.path.dirname(os.path.abspath(__file__))
    global pySrcDir
    global mmseqsPath
    ostype = platform.system()
    if ostype == 'Darwin':
        mmseqsPath = os.path.join(pySrcDir, 'bin/osx/mmseqs')
    elif ostype == 'Linux':
        mmseqsPath = os.path.join(pySrcDir, 'bin/linux/mmseqs')
    if not os.path.isfile(mmseqsPath):
        sys.stderr.write('\nERROR: the path to mmseqs is not valid, please run sonicparanoid.py.')

    '''
    global cfgPath
    import json
    with open(cfgPath, 'r') as f:
        data = json.load(f)
        mmseqsPath = data['mmseqs']
        if mmseqsPath == 'mmseqs':
            sys.stderr.write('\nERROR: the mpath to mmseqs is not correct, please run sonicparanoid.py.')
    '''
    return mmseqsPath



def get_pckg_root():
    """Return the path to the directory with the third-party used software tools."""
    return pckg_root



def get_parser():
    """Return the parser blast output."""
    return blast_parser



def get_quick_multiparanoid_src_dir():
    """Return the directory in which the binaries and source of quiclparanoid are stored."""
    return multiparanoidSrcDir



def load_input_sequences(inSeq, debug=False):
    """Load input sequences in dictionary."""
    if debug:
        print('load_input_sequences :: START')
        print('Input file:\t%s'%inSeq)
    outDict = OrderedDict()
    #Use biopython
    from Bio import SeqIO
    cnt = wcnt = 0
    for record in SeqIO.parse(open(inSeq), 'fasta'): #python2.7, 3
        cnt += 1
        seq = str(record.seq)
        #seqId = record.id
        seqId = record.id.split(' ', 1)[0]
        #add sequecens to the dictionary
        outDict[seqId] = seq.lower()
    if debug:
        print('%d fasta sequences loaded in dictionary.'%len(outDict))
    return outDict



def mmseqs_createdb(inSeq, outDir=os.getcwd(), debug=False):
    """Create a database file for mmseqs2 from the input sequence file."""
    if debug:
        print('mmseqs_createdb :: START')
        print('Input FASTA file:\t%s'%inSeq)
        print('Outdir:\t%s'%outDir)
    #check that the input file and the database exist
    if not os.path.isfile(inSeq):
        sys.stderr.write('The file %s was not found, please provide the path to a valid FASTA file'%inSeq)
        sys.exit(-2)
    #check if the database exists
    if outDir[-1] != '/':
        outDir += '/'
    # create dir if not already exists
    systools.makedir(outDir)
    # check the set db name
    dbName = os.path.basename(inSeq)
    dbName = dbName.split('.')[0] # take the left part of the file name
    dbName = '%s.mmseqs2db'%dbName
    dbPath = '%s%s'%(outDir, dbName)
    # command to be executed
    # EXAMPLE; mmseqs createdb in.fasta /outdir/mydb
    #makeDbCmd = 'mmseqs createdb %s %s'%(inSeq, dbPath)
    makeDbCmd = '%s createdb %s %s'%(get_mmseqs_path(), inSeq, dbPath)
    if debug:
        print('mmseqs2 createdb CMD:\t%s'%makeDbCmd)
    #execute the system call
    process = subprocess.Popen(makeDbCmd, shell=True, stdout=subprocess.PIPE, stderr=subprocess.PIPE)
    stdout_val, stderr_val = process.communicate() #get stdout and stderr
    process.wait()
    if debug:
        print('STDOUT:\n%s\n'%stdout_val)
        print('STDERR:\n%s\n'%stderr_val)
    #return a tuple with the results
    return(stdout_val, stderr_val, makeDbCmd, dbPath)



def mmseqs_createindex(dbPath, debug=False):
    """Create a index from a mmseq2 database file."""
    if debug:
        print('mmseqs_createindex :: START')
        print('Input mmseqs2 db file:\t%s'%dbPath)
    #check that the database file exist
    if not os.path.isfile(dbPath):
        sys.stderr.write('The file %s was not found, please provide the path to a mmseqs2 database file'%dbPath)
        sys.exit(-2)
    # command to be executed
    # EXAMPLE; mmseqs createindex in.mmseqs2_db
    #makeIdxCmd = 'mmseqs createindex %s '%(dbPath)
    makeIdxCmd = '%s createindex %s '%(get_mmseqs_path(), dbPath)
    if debug:
        print('mmseqs2 createindex CMD:\t%s'%makeIdxCmd)
    #execute the system call
    process = subprocess.Popen(makeIdxCmd, shell=True, stdout=subprocess.PIPE, stderr=subprocess.PIPE)
    stdout_val, stderr_val = process.communicate() #get stdout and stderr
    process.wait()
    if debug:
        print('STDOUT:\n%s\n'%stdout_val)
        print('STDERR:\n%s\n'%stderr_val)
    # make sure that the 3 idx files have been properly created
    idx1 = '%s.sk6'%dbPath
    if not os.path.isfile(idx1):
        sys.stderr.write('The mmseqs2 index file %s could not be created.'%idx1)
        sys.exit(-2)
    idx2 = '%s.sk6.index'%dbPath
    if not os.path.isfile(idx2):
        sys.stderr.write('The mmseqs2 index file %s could not be created.'%idx2)
        sys.exit(-2)
    idx3 = '%s.sk6.mmseqsindex'%dbPath
    if not os.path.isfile(idx3):
        sys.stderr.write('The mmseqs2 index file %s could not be created.'%idx3)
        sys.exit(-2)
    # return a output tuple
    return(stdout_val, stderr_val, makeIdxCmd, idx1, idx2, idx3)



def mmseqs_cleanup(inDir=os.getcwd(), tmpDirPath=None, debug=False):
    """Remove all files related to a mmseqs2 database and indexes"""
    import fnmatch
    # check input dir name
    #if inDir[-1] != '/':
        #inDir += '/'
    if tmpDirPath is None:
        sys.stderr.write('\nERROR: MMseqs2 temporary directory was not specified.\n')
        sys.exit(-2)
    # extract the alignment name from the tmp dir name
    alignName = os.path.basename(tmpDirPath).split('_', 1)[-1] # take the right part

    #sys.exit('DEBUG :: mmseqs_cleanup [first part]')
    #search for the files
    for f in os.listdir(inDir):
        if fnmatch.fnmatch(f, 'mmseqs2*.{:s}*'.format(alignName)):
            os.remove('%s%s'%(inDir, f))
    if os.path.isdir(tmpDirPath):
        shutil.rmtree(tmpDirPath)



def mmseqs_search(inSeq, dbSeq, dbDir=os.getcwd(), outDir=os.getcwd(), tmpDirName=None, sensitivity=4.0, evalue=1000, threads=4, cleanUp=False, debug=False):
    """Align protein sequences using mmseqs2."""
    if debug:
        print('\nmmseqs_search :: START')
        print('Input query FASTA file:\t%s'%inSeq)
        print('Input target FASTA file:\t%s'%dbSeq)
        print('mmseqs2 database directory:\t%s'%dbDir)
        print('Output directory:\t%s'%outDir)
        print('MMseqs2 tmp directory:\t{:s}'.format(tmpDirName))
        print('MMseqs2 sensitivity (-s):\t%s'%str(sensitivity))
        print('Threads:\t%d'%threads)
        print('Remove temporary files:\t%s'%cleanUp)
    #check that the input file and the database exist
    if not os.path.isfile(inSeq):
        sys.stderr.write('The query file %s was not found, please provide the path to a valid FASTA file'%inSeq)
        sys.exit(-2)
    if not os.path.isfile(dbSeq):
        sys.stderr.write('The target file %s was not found, please provide the path to a valid FASTA file'%dbSeq)
        sys.exit(-2)
    # check sensitivity
    if (sensitivity < 1) or sensitivity > 8.5:
        sys.stderr.write('\nERROR: the sensitivity value for MMseqs2.0 must be a value between 1.0 and 8.5.\n')
        sys.exit(-5)
    # create directory if not previously created
    systools.makedir(outDir)
    systools.makedir(dbDir)
    # set the tmp dir
    tmpDir = None
    if tmpDirName is None:
        tmpDir = '%stmp_mmseqs/'%outDir
    else:
        tmpDir = '{:s}{:s}/'.format(outDir, tmpDirName)
    systools.makedir(tmpDir)
    # check the query db name
    queryDBname = os.path.basename(inSeq)
    queryDBname = queryDBname.split('.')[0] # take the left part of the file name
    queryDBname = '%s.mmseqs2db'%queryDBname
    queryDBpath = '%s%s'%(dbDir, queryDBname)
    # create the database if does not exist yet
    if not os.path.isfile(queryDBpath):
        mmseqs_createdb(inSeq, outDir=dbDir, debug=debug)
        #mmseqs_createindex(queryDBpath, debug=debug)
    # check the target db name
    targetDBname = os.path.basename(dbSeq)
    targetDBname = targetDBname.split('.')[0] # take the left part of the file name
    targetDBname = '%s.mmseqs2db'%targetDBname
    targetDBpath = '%s%s'%(dbDir, targetDBname)
    # create the database if does not exist yet
    if not os.path.isfile(targetDBpath):
        mmseqs_createdb(dbSeq, outDir=dbDir, debug=debug)
        #mmseqs_createindex(targetDBpath, debug=debug)
    # set output name
    pairName = '%s-%s'%(os.path.basename(inSeq), os.path.basename(dbSeq))
    rawOutName = 'mmseqs2raw.%s'%pairName
    rawOutPath = '%s%s'%(outDir, rawOutName)
    blastOutName = 'mmseqs2blast.%s'%pairName
    blastOutPath = '%s%s'%(outDir, blastOutName)
    # command to be executed
    # EXAMPLE; mmseqs search queryDBfile targetDBfile outputFile tmpDir -s 7.5 -e 100000 --theads threads
    searchCmd = '%s search %s %s %s %s -s %s --threads %d'%(get_mmseqs_path(), queryDBpath, targetDBpath, rawOutPath, tmpDir, str(sensitivity), threads)
    if debug:
        print('mmseqs2 search CMD:\t%s'%searchCmd)
    #execute the system call
    process = subprocess.Popen(searchCmd, shell=True, stdout=subprocess.PIPE, stderr=subprocess.PIPE)
    stdout_val, stderr_val = process.communicate() #get stdout and stderr
    process.wait()
    if debug:
        print('STDOUT:\n%s\n'%stdout_val)
        print('STDERR:\n%s\n'%stderr_val)
    # convert the output to tab-separated BLAST output
    # EXAMPLE; mmseqs convertalis query.db target.db query_target_rawout query_target_blastout
    #convertCmd = 'mmseqs convertalis %s %s %s %s'%(queryDBpath, targetDBpath, rawOutPath, blastOutPath)
    convertCmd = '%s convertalis %s %s %s %s'%(get_mmseqs_path(), queryDBpath, targetDBpath, rawOutPath, blastOutPath)
    if debug:
        print('mmseqs2 convertalis CMD:\t%s'%convertCmd)
    #execute the system call
    process = subprocess.Popen(convertCmd, shell=True, stdout=subprocess.PIPE, stderr=subprocess.PIPE)
    stdout_val, stderr_val = process.communicate() #get stdout and stderr
    process.wait()
    if debug:
        print('STDOUT:\n%s\n'%stdout_val)
        print('STDERR:\n%s\n'%stderr_val)
    # cleanup output directory
    if cleanUp:
        mmseqs_cleanup(inDir=outDir, debug=debug)
    #sys.exit('DEBUG :: mmseqs_search')
    #return a tuple with the results
    return blastOutPath



def mmseqs_1pass(inSeq, dbSeq, dbDir=os.getcwd(), outDir=os.getcwd(), tmpDirName=None, sensitivity=4.0, evalue=1000, cutoff=40, threads=4, debug=False):
    """Execute the 1-pass alignment mmseqs2 similar to the one implemented in core-inparanoid."""
    if debug:
        print('\nmmseqs_1pass :: START')
        print('Input query FASTA file:\t%s'%inSeq)
        print('Input target FASTA file:\t%s'%dbSeq)
        print('mmseqs2 database directory:\t%s'%dbDir)
        print('Output directory:\t%s'%outDir)
        print('MMseqs2 tmp directory:\t{:s}'.format(tmpDirName))
        print('MMseqs2 sensitivity (-s):\t%s'%str(sensitivity))
        print('Bitscore cutoff:\t%d'%cutoff)
        print('Threads:\t%d'%threads)
    #start the timing which will also include the time for the index, database creation (if required) and parsing
    start_time = time.time()
    # create mmseqs alignment conveted into blastp tab-separated format
    blastLikeOutput = mmseqs_search(inSeq, dbSeq, dbDir=dbDir, outDir=outDir, tmpDirName=tmpDirName, sensitivity=sensitivity, evalue=1000, threads=threads, cleanUp=False, debug=debug)
    parserPath = '%smmseqs_parser_cython.py'%outDir
    prevDir = os.getcwd()
    os.chdir(outDir)
    # check cutoff
    if cutoff < 30:
        cutoff = 40
    # prepare now the parsing
    # EXAMPLE: python3 mmseqs_parser_cython.py --input mmseqs2blast.A-B --query A --db B --output A-B --cutoff 40
    parsedOutput = blastLikeOutput.replace('mmseqs2blast.', '')
    parseCmd = 'python3 %s --input %s --query %s --db %s --output %s --cutoff %d'%(parserPath, blastLikeOutput, inSeq, dbSeq, parsedOutput, cutoff)
    if debug:
        print('Parse CMD:\t%s'%parseCmd)
    #execute the system call
    process = subprocess.Popen(parseCmd, shell=True, stdout=subprocess.PIPE, stderr=subprocess.PIPE)
    stdout_val, stderr_val = process.communicate() #get stdout and stderr
    process.wait()
    if debug:
        print('STDOUT:\n%s\n'%stdout_val)
        print('STDERR:\n%s\n'%stderr_val)
    tot_time = round(time.time() - start_time, 2)
    if debug:
        sys.stdout.write('\nMMseqs2 alignment and parsing elapsed time (seconds):\t%s\n'%str(tot_time))
    # now it possible to remove the temporary files...
    mmseqs_cleanup(inDir=outDir, tmpDirPath=os.path.join(outDir, tmpDirName), debug=debug)
    # reset original working directory
    os.chdir(prevDir)
    #sys.exit('DEBUG :: mmseqs_1pass')
    return (parsedOutput, tot_time, 0, tot_time)



def prettify_multispecies_output(inTbl, outDir=os.getcwd(), refSpeciesList=[], minScore=0.05, maxGenePerSp=10, debug=False):
    """Prettify the output from quickparanoid and make it more readable."""
    #the origninal input table contains these information:
    #clusterID species gene is_seed_ortholog confidence_score species_in_cluster tree_conflict
    #species_in_cluster and tree_conflict are the same for each entry pf the same clstr
    if debug:
        print('prettify_multispecies_output :: START')
        print('Input table:%s'%inTbl)
        print('Outdir:%s'%outDir)
        print('Species:\t%d'%len(refSpeciesList))
        print('Minimum homolog score:\t%s'%str(minScore))
        print('Maximum number of genes per species:\t{:d}'.format(maxGenePerSp))
    outDict = OrderedDict()
    refSpeciesList.sort()
    if outDir[-1] != '/':
        outDir = '%s/'%outDir
    #start extracting the information
    for ln in open(inTbl):
        if (ln[0] == '#') or (ln[0] == 'c'):
            continue
        ln = ln.rstrip('\n')
        clstrId, sp, gene, isSeedOrtho, score, speciesList, conflict = ln.split('\t')
        clstrId = int(clstrId)
        score = float(score)
        speciesList = speciesList.split('-')
        isSeedOrtho = int(isSeedOrtho)
        if conflict[-2:] == 'rs':
            conflict = 'nr'
        elif conflict[-2:] == 'es':
            conflict = 'nm'
        else:
            conflict = 'no'
        #create the entry in the dictionary
        if not clstrId in outDict:
            #contains: number of species, number of seed orthologs, avg clstr score, dict with genes for each score, conflict type
            outDict[clstrId] = [len(speciesList), 1, score, score, isSeedOrtho, OrderedDict([(sp, OrderedDict([(gene, score)]))]), conflict]
        else:
            # skip entries with score lower than the threshold
            if score < minScore:
                continue
            #update the dictionary with the species and associated genes
            if sp in outDict[clstrId][5]:
                # if the number of genes per species is too high, just skip it
                if len(outDict[clstrId][5][sp]) >= maxGenePerSp:
                    continue
                if not gene in outDict[clstrId][5][sp]: #if the gene is not present already
                    outDict[clstrId][5][sp][gene] = score
                else:
                    sys.stderr.write('\nERROR: attemp of multiple entry for gene %s for species %s\n'%(gene, sp))
            else: #add the new species to the species dictionary
                outDict[clstrId][5][sp] = OrderedDict([(gene, score)])
            # update the other fields
            outDict[clstrId][1] += 1 #increment the entries
            outDict[clstrId][2] += score #increment the total confidence
            outDict[clstrId][3] = (outDict[clstrId][2])/float(outDict[clstrId][1]) #update average score
            outDict[clstrId][4] += isSeedOrtho #incrememnt the seed orthologs count

            ''' WORKING VERSION WITH NO LIMIT ON #GENE PER SPECIES
            outDict[clstrId][1] += 1 #increment the entries
            outDict[clstrId][2] += score #increment the total confidence
            outDict[clstrId][3] = (outDict[clstrId][2])/float(outDict[clstrId][1]) #update average score
            outDict[clstrId][4] += isSeedOrtho #incrememnt the seed orthologs count
            #update the dictionary with the species and associated genes
            if sp in outDict[clstrId][5]:
                if not gene in outDict[clstrId][5][sp]: #if the gene is not present already
                    outDict[clstrId][5][sp][gene] = score
                else:
                    sys.stderr.write('\nERROR: attempt of multiple entry for gene %s for species %s\n'%(gene, sp))
            else: #add the new species to the species dictionary
                outDict[clstrId][5][sp] = OrderedDict([(gene, score)])
            '''
    #create datapoints file with the count for each cluster size (number of species in clstr)
    bsName = os.path.basename(inTbl)
    bsName = bsName.rsplit('.')[0]
    mainOutPath = '%stmp_%s.tsv'%(outDir, bsName)
    #write the main output table
    ofd = open(mainOutPath, 'w')
    spNamesInHdr = ['%s\tavg_score_sp%d'%(x, i+1) for i, x in enumerate(refSpeciesList)]
    ofd.write('group_id\tgroup_size\tsp_in_grp\tseed_ortholog_cnt\t%s\tconflict\n'%('\t'.join(spNamesInHdr)))
    del spNamesInHdr
    #sort the tmpDict by size
    for k in outDict:
        size = outDict[k][0]
        entries = outDict[k][1]
        avg_score = str(round(outDict[k][3], 3))
        #sort the dictionary of each species by score
        spGenesDict = outDict[k][5]
        tmpGenesPerSpecies = [] # will contain the elemented for the final string
        #generate the string with genes and scores per species
        for spName in refSpeciesList:
            if not spName in spGenesDict:
                #then create and empty string
                tmpGenesPerSpecies.append('*\t0')
            else: #add the informations
                avgScoreVal = '1'
                confValues = list(spGenesDict[spName].values())
                geneNames = list(spGenesDict[spName].keys())
                if len(spGenesDict[spName]) > 1:
                    #calculate the average score
                    confValues = list(spGenesDict[spName].values())
                    avgScoreVal = str(round(np.mean(confValues), 3))
                #create the substring with the info about specis spName
                tmpSubStrList = []
                for i, gname in enumerate(geneNames):
                    if confValues[i] == 1.:
                        tmpSubStrList.append('%s'%(gname))
                    else:
                        tmpSubStrList.append('%s:%s'%(gname, str(confValues[i])))
                #join the substring
                joinedSubstr = ','.join(tmpSubStrList)
                tmpGenesPerSpecies.append('%s\t%s'%(joinedSubstr, avgScoreVal))
        # extract the variables that will be printed
        seedCnt = outDict[k][4]
        conflict = outDict[k][6]
        ofd.write('%d\t%d\t%d\t%d\t%s\t%s\n'%(k, entries, len(spGenesDict), seedCnt, '\t'.join(tmpGenesPerSpecies), conflict))
    ofd.close()
    #print some summary about the dictionary
    if debug:
        print('Total clstrs:\t%d'%(len(outDict)))
        print('Output file:\t%s'%(mainOutPath))
    return mainOutPath



def update_database(inDir, rootDir=os.getcwd(), dbDir=os.getcwd(), updateId=None, sensitivity=4.0, cutoff=40, confCutoff=0.05, lenDiffThr=0.5, threads=8, debug=True):
    """Check the runs that are required to upgrade the ortholog database with new species."""
    import platform
    if debug:
        print('update_database :: START')
        print('Input directory:\t%s'%inDir)
        print('Root directory:\t%s'%rootDir)
        print('MMseqs2 database directory:\t%s'%dbDir)
        print('ID for the update:\t%s'%updateId)
        print('MMseqs2 sensitivity (-s):\t%s'%str(sensitivity))
        print('Bitscore cutoff:\t%d'%cutoff)
        print('Confidence cutoff for paralogs:\t%s'%str(confCutoff))
        print('Length difference filtering threshold:\t%s'%str(lenDiffThr))
        print('Threads:\t%d'%threads)
    #check that the input directory is valid
    if not os.path.isdir(inDir):
        sys.stderr.write('ERROR: the directory containing the input proteomes \n%s\n does not exist.\n'%inDir)
        sys.exit(-2)
    #root directory
    if not os.path.isdir(rootDir):
        sys.stderr.write('ERROR: the directory containing the SonicParanoid runs \n%s\n does not exist.\n'%rootDir)
        sys.exit(-2)
    if updateId is None:
        sys.stderr.write('ERROR: you must provide an name for the update.\n')
        sys.exit(-2)
    # set the database directory path
    if dbDir[-1] != '/':
        dbDir += '/'
    # create database directory if not previously created
    systools.makedir(dbDir)
    #check the operative system
    pckgDir = get_pckg_root()
    quickParanoidPckg = None
    tmpFlist = os.listdir(inDir)
    #create species and pairs lists
    if len(tmpFlist) < 3:
        sys.stderr.write('ERROR: the directory with the input files only contains %d (%s) files\n Please provide at least 3 genomes.\n'%(len(tmpFlist), ', '.join(tmpFlist)))
        sys.exit(-2)
    #make sure that all the file do not contain file extensions nor '-' in their names
    for f in tmpFlist:
        if f.startswith('.DS_'):
            continue
        if '-' in f:
            sys.stderr.write('ERROR: the file names including the \'-\' symbol (%s) are not allowed.\n Please remove the \'-\' from the species names.\n'%(f))
            sys.exit(-2)
        if '.' in f:
            sys.stderr.write('ERROR: the file (%s) includes the \'.\' symbol. This might reduce the readability of the output file names.\nPlease remove \'.\' from your input species names.\n'%(f))
            sys.exit(-2)
    tmpFlist.sort() #this to avoid direfferent behaviour in OSX and Linux
    #check the existance if the shared directory
    sharedDir = '%sshared_output/'%rootDir
    if not os.path.isdir(sharedDir):
        systools.makedir(sharedDir)
        if debug:
            print('Shared directory created in %s'%sharedDir)
    # set the path to the quickparanoid package based on operative system
    ostype = platform.system()
    if ostype == 'Darwin':
        if '.DS_Store' in tmpFlist:
            tmpFlist.remove('.DS_Store')
        quickParanoidPckg = '%squickparanoid_osx.tar.gz'%pckgDir
    elif ostype == 'Linux':
        quickParanoidPckg = '%squickparanoid_redhat.tar.gz'%pckgDir
    #create the file with species names
    spFile = '%sspecies.txt'%(rootDir)
    spList = []
    ofd = open(spFile, 'w')
    for f in tmpFlist:
        ofd.write('%s\n'%f)
        spList.append(f)
    ofd.close()
    del tmpFlist
    #generate file with the combinations
    spPairsFile = '%sspecies_pairs.txt'%(rootDir)
    spPairs = list(itertools.combinations(spList, r=2))
    spPairs.sort()
    #give some information about the combinations
    dashedPairs = ['%s-%s'%(tpl[0], tpl[1]) for tpl in spPairs]
    #check that the file with genome pairs has not been created yet
    ofd = open(spPairsFile, 'w')
    #[ofd.write('%s-%s\n'%(tpl[0], tpl[1])) for tpl in spPairs]
    [ofd.write('%s\n'%(el)) for el in dashedPairs]
    ofd.close()
    #check which pairs are already available
    rootSubdirs = os.listdir(rootDir)
    if debug:
        print('\nRoot subdirs:')
        print(rootSubdirs)
        print(dashedPairs)
    #make list with required and reusable runs
    completedRuns = []
    runsToFix = []
    newRuns = []
    #check the required run, those that need to be fixed and new runs
    for dname in rootSubdirs:
        #print(dname)
        if '-' in dname:
            sp1, sp2 = dname.split('-')
            if dname in dashedPairs:
                okRun, missingComp = check_2species_run(rootDir, sp1, sp2, sharedDir, debug=False)
                if len(missingComp) == 0 and okRun:
                    completedRuns.append(dname)
                else:
                    if debug:
                        print(dname)
                    runsToFix.append((dname, okRun, missingComp))
            inverted = '%s-%s'%(sp2, sp1)
            if inverted in dashedPairs:
                print('pair inverted!')
                okRun, missingComp = check_2species_run(rootDir, sp1, sp2, sharedDir, debug=False)
                if len(missingComp) == 0 and okRun:
                    completedRuns.append(dname)
                else:
                    runsToFix.append((dname, okRun, missingComp))
    print('\nFor the %d input species %d combinations are possible.'%(len(spList), len(spPairs)))
    print('Runs already completed:\t%d'%len(completedRuns))
    print('Runs to be fixed (partial or complete rerun):\t%d'%len(runsToFix))
    print('Required runs:\t%d'%(len(dashedPairs) - len(completedRuns) ) )
    #sys.exit('debug :: update_database')
    #start fixing the runs that need to be fixed
    for tpl in runsToFix:
        pair, status, missingComp = tpl
        sp1, sp2 = pair.split('-')
        f1 = '%s%s'%(inDir, sp1)
        f2 = '%s%s'%(inDir, sp2)
        #sys.exit('debug')
        if len(missingComp) == 0: #perform only ortholog search
            outTable, outSql, outSummary, tot_time, okRun = run_sonicparanoid2_parallel_mmseqs(f1, f2, outDir='%s%s-%s/'%(rootDir, sp1, sp2), threads=threads, sharedDir=sharedDir, mmseqsDbDir=dbDir, sensitivity=sensitivity, cutoff=cutoff, confCutoff=confCutoff, lenDiffThr=lenDiffThr, reuse=True, noMmseqs=True, debug=debug)
        else: #perform mmseqs2 and ortholog search
            outTable, outSql, outSummary, tot_time, okRun = run_sonicparanoid2_parallel_mmseqs(f1, f2, outDir='%s%s-%s/'%(rootDir, sp1, sp2), threads=threads, sharedDir=sharedDir, mmseqsDbDir=dbDir, sensitivity=sensitivity, cutoff=cutoff, confCutoff=confCutoff, lenDiffThr=lenDiffThr, reuse=True, noMmseqs=False, debug=debug)
        #add the pair to the completed run list
        completedRuns.append(pair)
    if debug:
        print('All the broken %d runs have been fixed.'%len(runsToFix))
        print('Runs already completed:\t%d'%len(completedRuns))
        print('Required runs to the final update:\t%d'%(len(dashedPairs) - len(completedRuns) ) )
    #execute sonicparanoid for the remaining runs
    spFile, pairsFile = run_sonicparanoid2(inDir, outDir=rootDir, threads=threads, sharedDir=sharedDir, mmseqsDbDir=dbDir, sensitivity=sensitivity, cutoff=cutoff, reuse=True, noMmseqs=False, overwrite=False, debug=debug)
    #create the multi species clustering
    multiSpDir = '%smulti_species_%s'%(rootDir, updateId)
    # All
    sqlPaths = fetch_sql_files(rootDir=rootDir, outDir=multiSpDir, pairsFile=pairsFile, coreOnly=False, debug=debug)
    print('SQL tables in update run (%s) All:\t%d'%(updateId, len(sqlPaths)))
    #execute quickParanoid
    multiparanoidRoot = get_quick_multiparanoid_src_dir()
    run_quickparanoid(sqlTblDir=multiSpDir, outDir=multiSpDir, srcDir=multiparanoidRoot, outName='%s_all.tsv'%updateId, speciesFile=spFile, debug=debug)
    # CORE
    sqlPaths = fetch_sql_files(rootDir=rootDir, outDir=multiSpDir, pairsFile=pairsFile, coreOnly=True, debug=debug)
    print('SQL tables in update run (%s) CORE:\t%d'%(updateId, len(sqlPaths)))
    #execute quickParanoid
    run_quickparanoid(sqlTblDir=multiSpDir, outDir=multiSpDir, srcDir=multiparanoidRoot, outName='%s_core.tsv'%updateId, speciesFile=spFile, debug=debug)



def single_query_blast(hitsDict, queryDict, dbDict, scoreCoff=40, dbsize=None, outName=None, outDir=os.getcwd(), tmpSuffix='1', debug=False):
    """
    Execute 2-pass blast for single query sequences.

    The db files will contain all the hits for the single sequence query in the previous blast run
    """
    if debug:
        print('single_query_blast :: START')
        print('Hits dictionary size:\t%d'%len(hitsDict))
        print('Input sequences:\t%s'%len(queryDict))
        print('DB sequences:\t%s'%len(dbDict))
        print('Score cutoff:\t%d'%scoreCoff)
        print('Output name:\t%s'%outName)
        print('Output dir:\t%s'%outDir)
        print('Tmp suffix:\t%s'%tmpSuffix)
    tmpDir = '%stmp/'%(outDir)
    if not os.path.isdir(tmpDir):
        systools.makedir(tmpDir)
    tmpQuery = 'tmpq_%s'%(tmpSuffix)
    tmpDB = 'tmpd_%s'%(tmpSuffix)
    #dbsize = 5000000
    bparser = '%sblast_parser.pl'%outDir
    matrix = 'BLOSUM62'
    import subprocess
    hitsDictKeys = list(hitsDict.keys())
    for q in hitsDictKeys:
        #dbIds = hitsDict[q].split(' ')
        dbIds = hitsDict[q]
        #create the small database file
        tmpFd = open('%s%s'%(tmpDir, tmpDB), 'w')
        for hitId in dbIds:
            tmpFd.write('>%s\n%s\n'%(hitId, dbDict[hitId]))
        tmpFd.close()
        #create the blast database
        makeDbCmd = 'makeblastdb -in %s%s -dbtype prot -out %s%s -title %s'%(tmpDir, tmpDB, tmpDir, tmpDB, tmpDB)
        process = subprocess.Popen(makeDbCmd, shell=True, stdout=subprocess.PIPE, stderr=subprocess.PIPE)
        process.wait()
        #create the query sequence file
        tmpFd = open('%s%s'%(tmpDir, tmpQuery), 'w')
        tmpFd.write('>%s\n%s\n'%(q, queryDict[q]))
        tmpFd.close()
        #remove the query sequence since it will not be used anymore
        del queryDict[q]
        #Execute blast using only one cpu
        # num_alignment, dbsize
        #blastpCmd = 'blastp -num_threads 1 -seg no -query %s%s -db %s%s -num_alignments %d -dbsize %d -outfmt 5'%(tmpDir, tmpQuery, tmpDir, tmpDB, len(dbIds), dbsize)
        # num_alignment
        #blastpCmd = 'blastp -num_threads 1 -seg no -query %s%s -db %s%s -num_alignments %d -outfmt 5'%(tmpDir, tmpQuery, tmpDir, tmpDB, len(dbIds))
        # dbsize
        ##blastpCmd = 'blastp -num_threads 1 -seg no -query %s%s -db %s%s -dbsize %d -outfmt 5'%(tmpDir, tmpQuery, tmpDir, tmpDB, dbsize)
        # dbsize 5000000
        ###blastpCmd = 'blastp -num_threads 1 -seg no -query %s%s -db %s%s -dbsize 5000000 -outfmt 5'%(tmpDir, tmpQuery, tmpDir, tmpDB)
        # no extra options
        blastpCmd = 'blastp -num_threads 1 -seg no -query %s%s -db %s%s -outfmt 5'%(tmpDir, tmpQuery, tmpDir, tmpDB)
        parseBlastCmd = '%s %d >> %s%s'%(bparser, scoreCoff, outDir, outName)
        #execute the subprocesses
        blastpProc = subprocess.Popen(blastpCmd, shell=True, stdout=subprocess.PIPE, stderr=subprocess.PIPE)
        parseBlastProc = subprocess.Popen(parseBlastCmd, shell=True, stdin=blastpProc.stdout, stdout=subprocess.PIPE, stderr=subprocess.PIPE)
        blastpProc.stdout.close()
        stdout_val, stderr_val = parseBlastProc.communicate()
        blastpProc.wait()
        #remove the query from the dictionary (hitsDict)
        del hitsDict[q]



def single_query_blast_slice(hitsSlice, hitsDict, queryDict, dbDict, scoreCoff=40, outName=None, outDir=os.getcwd(), tmpSuffix='1', debug=False):
    """
    Execute 2-pass blast for single query sequences on a subset of the hits.

    The db files will contain all the hits for the single sequence query in the previous blast run
    """
    if debug:
        print('single_query_blast_slice :: START')
        print('Hits slice size:\t%d'%len(hitsSlice))
        print('Hits dictionary size:\t%d'%len(hitsDict))
        print('Input sequences:\t%s'%len(queryDict))
        print('DB sequences:\t%s'%len(dbDict))
        print('Score cutoff:\t%d'%scoreCoff)
        print('Output name:\t%s'%outName)
        print('Output dir:\t%s'%outDir)
        print('Tmp suffix:\t%s'%tmpSuffix)
    tmpDir = '%stmp/'%(outDir)
    if not os.path.isdir(tmpDir):
        systools.makedir(tmpDir)
    tmpQuery = 'tmpq_%s'%(tmpSuffix)
    tmpDB = 'tmpd_%s'%(tmpSuffix)
    dbsize = 5000000
    bparser = '%sblast_parser.pl'%outDir
    matrix = 'BLOSUM62'
    import subprocess
    for q in hitsSlice:
        #dbIds = hitsDict[q].split(' ')
        dbIds = hitsDict[q]
        #create the small database file
        tmpFd = open('%s%s'%(tmpDir, tmpDB), 'w')
        for hitId in dbIds:
            #print(hitId)
            tmpFd.write('>%s\n%s\n'%(hitId, dbDict[hitId]))
        tmpFd.close()
        #create the blast database
        makeDbCmd = 'makeblastdb -in %s%s -dbtype prot -out %s%s -title %s'%(tmpDir, tmpDB, tmpDir, tmpDB, tmpDB)
        process = subprocess.Popen(makeDbCmd, shell=True, stdout=subprocess.PIPE, stderr=subprocess.PIPE)
        process.wait()
        #create the query sequence file
        tmpFd = open('%s%s'%(tmpDir, tmpQuery), 'w')
        tmpFd.write('>%s\n%s\n'%(q, queryDict[q]))
        tmpFd.close()
        del queryDict[q]
        #Execute blast using only one cpu
        blastpCmd = 'blastp -num_threads 1 -seg no -query %s%s -db %s%s -num_alignments %d -matrix %s -dbsize %d -outfmt 5'%(tmpDir, tmpQuery, tmpDir, tmpDB, len(dbIds), matrix, dbsize)
        parseBlastCmd = '%s %d >> %s%s_%s'%(bparser, scoreCoff, outDir, outName, tmpSuffix)
        #execute the subprocesses
        blastpProc = subprocess.Popen(blastpCmd, shell=True, stdout=subprocess.PIPE, stderr=subprocess.PIPE)
        parseBlastProc = subprocess.Popen(parseBlastCmd, shell=True, stdin=blastpProc.stdout, stdout=subprocess.PIPE, stderr=subprocess.PIPE)
        blastpProc.stdout.close()
        stdout_val, stderr_val = parseBlastProc.communicate()
        blastpProc.wait()
        #remove the query from the dictionary (hitsDict)
        del hitsDict[q]
    return '%s%s%s'%(outDir, outName, tmpSuffix)



def single_query_blast_slice_dict_args(paramsDict):
    """
    Execute 2-pass blast for single query sequences on a subset of the hits.

    The input is a dictionary containing all the required parameters for the function to be executed.
    The db files will contain all the hits for the single sequence query in the previous blast run
    """
    hitsSlice = paramsDict['hitsSlice']
    hitsDict = paramsDict['hitsDict']
    queryDict = paramsDict['queryDict']
    dbDict = paramsDict['dbDict']
    scoreCoff = paramsDict['scoreCoff']
    outName = paramsDict['outName']
    outDir = paramsDict['outDir']
    tmpSuffix = paramsDict['tmpSuffix']
    debug = paramsDict['debug']
    if debug:
        print('single_query_blast_slice_dict_args :: START')
        print('Hits slice size:\t%d'%len(hitsSlice))
        print('Hits dictionary size:\t%d'%len(hitsDict))
        print('Input sequences:\t%s'%len(queryDict))
        print('DB sequences:\t%s'%len(dbDict))
        print('Score cutoff:\t%d'%scoreCoff)
        print('Output name:\t%s'%outName)
        print('Output dir:\t%s'%outDir)
        print('Tmp suffix:\t%s'%tmpSuffix)
    tmpDir = '%stmp/'%(outDir)
    if not os.path.isdir(tmpDir):
        systools.makedir(tmpDir)
    tmpQuery = 'tmpq_%s'%(tmpSuffix)
    tmpDB = 'tmpd_%s'%(tmpSuffix)
    #dbsize = 5000000
    #dbsize = 100000
    bparser = '%sblast_parser.pl'%outDir
    matrix = 'BLOSUM62'
    import subprocess
    for q in hitsSlice:
        dbIds = hitsDict[q]
        #create the small database file
        tmpFd = open('%s%s'%(tmpDir, tmpDB), 'w')
        for hitId in dbIds:
            tmpFd.write('>%s\n%s\n'%(hitId, dbDict[hitId]))
        tmpFd.close()
        #create the blast database
        makeDbCmd = 'makeblastdb -in %s%s -dbtype prot -out %s%s -title %s'%(tmpDir, tmpDB, tmpDir, tmpDB, tmpDB)
        process = subprocess.Popen(makeDbCmd, shell=True, stdout=subprocess.PIPE, stderr=subprocess.PIPE)
        process.wait()
        #create the query sequence file
        tmpFd = open('%s%s'%(tmpDir, tmpQuery), 'w')
        tmpFd.write('>%s\n%s\n'%(q, queryDict[q]))
        tmpFd.close()
        #Execute blast using only one cpu
        #blastpCmd = 'blastp -num_threads 1 -seg no -query %s%s -db %s%s -num_alignments %d -matrix %s -dbsize %d -outfmt 5'%(tmpDir, tmpQuery, tmpDir, tmpDB, len(dbIds), matrix, dbsize)
        #blastpCmd = 'blastp -num_threads 1 -seg no -query %s%s -db %s%s -num_alignments %d -matrix %s -outfmt 5'%(tmpDir, tmpQuery, tmpDir, tmpDB, len(dbIds), matrix)
        #remove num_alignments and matrix
        blastpCmd = 'blastp -num_threads 1 -seg no -query %s%s -db %s%s -outfmt 5'%(tmpDir, tmpQuery, tmpDir, tmpDB)
        #-evalue 1e-9
        #blastpCmd = 'blastp -num_threads 1 -seg no -query %s%s -db %s%s -evalue 1e-3 -outfmt 5'%(tmpDir, tmpQuery, tmpDir, tmpDB)
        parseBlastCmd = '%s %d >> %s%s_%s'%(bparser, scoreCoff, outDir, outName, tmpSuffix)
        #execute the subprocesses
        blastpProc = subprocess.Popen(blastpCmd, shell=True, stdout=subprocess.PIPE, stderr=subprocess.PIPE)
        parseBlastProc = subprocess.Popen(parseBlastCmd, shell=True, stdin=blastpProc.stdout, stdout=subprocess.PIPE, stderr=subprocess.PIPE)
        blastpProc.stdout.close()
        stdout_val, stderr_val = parseBlastProc.communicate()
        blastpProc.wait()
        #remove the query from the dictionary (hitsDict)
        del hitsDict[q]
    #return the output file
    return '%s%s%s'%(outDir, outName, tmpSuffix)



def single_query_blastall_slice_dict_args(paramsDict):
    """
    Execute 2-pass blast for single query sequences on a subset of the hits.

    The input is a dictionary containing all the required parameters for the function to be executed.
    The db files will contain all the hits for the single sequence query in the previous blast run
    """
    hitsSlice = paramsDict['hitsSlice']
    hitsDict = paramsDict['hitsDict']
    queryDict = paramsDict['queryDict']
    dbDict = paramsDict['dbDict']
    scoreCoff = paramsDict['scoreCoff']
    outName = paramsDict['outName']
    outDir = paramsDict['outDir']
    tmpSuffix = paramsDict['tmpSuffix']
    debug = paramsDict['debug']
    if debug:
        print('single_query_blastall_slice :: START')
        print('Hits slice size:\t%d'%len(hitsSlice))
        print('Hits dictionary size:\t%d'%len(hitsDict))
        print('Input sequences:\t%s'%len(queryDict))
        print('DB sequences:\t%s'%len(dbDict))
        print('Score cutoff:\t%d'%scoreCoff)
        print('Output name:\t%s'%outName)
        print('Output dir:\t%s'%outDir)
        print('Tmp suffix:\t%s'%tmpSuffix)
    tmpDir = '%stmp/'%(outDir)
    if not os.path.isdir(tmpDir):
        systools.makedir(tmpDir)
    tmpQuery = 'tmpq_%s'%(tmpSuffix)
    tmpDB = 'tmpd_%s'%(tmpSuffix)
    dbsize = 5000000
    bparser = '%sblast_parser.pl'%outDir
    matrix = 'BLOSUM62'
    import subprocess
    for q in hitsSlice:
        dbIds = hitsDict[q]
        #create the small database file
        tmpFd = open('%s%s'%(tmpDir, tmpDB), 'w')
        for hitId in dbIds:
            tmpFd.write('>%s\n%s\n'%(hitId, dbDict[hitId]))
        tmpFd.close()
        #create the blast database
        makeDbCmd = 'formatdb -i %s%s'%(tmpDir, tmpDB)
        process = subprocess.Popen(makeDbCmd, shell=True, stdout=subprocess.PIPE, stderr=subprocess.PIPE)
        process.wait()
        #create the query sequence file
        tmpFd = open('%s%s'%(tmpDir, tmpQuery), 'w')
        tmpFd.write('>%s\n%s\n'%(q, queryDict[q]))
        tmpFd.close()
        del queryDict[q]
        #Execute blast using only one cpu
        #system ("$blastall -C0 -FF -i $tmpi -d $tmpd -p blastp -v $Fld[3] -b $Fld[3] -M $matrix -z 5000000 -m7 | ./$blastParser $score_cutoff >> $Fld[4]");
        blastpCmd = 'blastall -C0  -FF -i %s%s -d %s%s -p blastp -v %d -b %d -M %s -z %d -m 7'%(tmpDir, tmpQuery, tmpDir, tmpDB, len(dbIds), len(dbIds), matrix, dbsize)
        parseBlastCmd = '%s %d >> %s%s_%s'%(bparser, scoreCoff, outDir, outName, tmpSuffix)
        #execute the subprocesses
        blastpProc = subprocess.Popen(blastpCmd, shell=True, stdout=subprocess.PIPE, stderr=subprocess.PIPE)
        parseBlastProc = subprocess.Popen(parseBlastCmd, shell=True, stdin=blastpProc.stdout, stdout=subprocess.PIPE, stderr=subprocess.PIPE)
        blastpProc.stdout.close()
        stdout_val, stderr_val = parseBlastProc.communicate()
        blastpProc.wait()
        #remove the query from the dictionary (hitsDict)
        del hitsDict[q]
    #return the output file
    return '%s%s%s'%(outDir, outName, tmpSuffix)



def single_query_mem_blast_slice_dict_args(paramsDict):
    """
    Execute 2-pass blast for single query sequences on a subset of the hits.

    The input is a dictionary containing all the required parameters for the function to be executed.
    The blast will be executed in memory without the ceation of any intermediate file.
    """
    hitsSlice = paramsDict['hitsSlice']
    hitsDict = paramsDict['hitsDict']
    queryDict = paramsDict['queryDict']
    dbDict = paramsDict['dbDict']
    scoreCoff = paramsDict['scoreCoff']
    outName = paramsDict['outName']
    outDir = paramsDict['outDir']
    tmpSuffix = paramsDict['tmpSuffix']
    debug = paramsDict['debug']
    if debug:
        print('single_query_mem_blast_slice :: START')
        print('Hits slice size:\t%d'%len(hitsSlice))
        print('Hits dictionary size:\t%d'%len(hitsDict))
        print('Input sequences:\t%s'%len(queryDict))
        print('DB sequences:\t%s'%len(dbDict))
        print('Score cutoff:\t%d'%scoreCoff)
        print('Output name:\t%s'%outName)
        print('Output dir:\t%s'%outDir)
        print('Tmp suffix:\t%s'%tmpSuffix)
    tmpDir = '%stmp/'%(outDir)
    if not os.path.isdir(tmpDir):
        systools.makedir(tmpDir)
    tmpQuery = 'tmpq_%s'%(tmpSuffix)
    tmpDB = 'tmpd_%s'%(tmpSuffix)
    dbsize = 5000000
    bparser = '%sblast_parser.pl'%outDir
    matrix = 'BLOSUM62'
    #####################################
    # example of blastp execution in Memory
    #blastp -query <(echo -e '>sp|P59806|YA177_RHOBA\nTLRFSLENDEVKRLCAATGKSADQIRTEAQAAFAAG') -subject <(echo -e '>sp|P59806|YA177_RHOBA\nMTKTLHFDCLSGISGDMTLGALIDLGVSVDQIQQGLHSLNLPDLKLRTEEVKKCGF')  -outfmt 5 | ./blast_parser.pl <score_cutoff> >> <output_file>
    import subprocess
    #create the bash file with the blast runs
    blastRunCmd = 0
    memBlastSrc = '%smem_blast_%s_%s.sh'%(outDir, outName, tmpSuffix)
    bashFd = open(memBlastSrc, 'w')
    bashFd.write('#!/bin/bash\n')
    for q in hitsSlice:
        dbIds = hitsDict[q]
        #create the small database file
        sbjSeq = ''
        #tmpFd = open('%s%s'%(tmpDir, tmpDB), 'w')
        seqCnt = 0
        tmpList = []
        for hitId in dbIds:
            tmpList.append('>%s'%hitId)
            tmpList.append(dbDict[hitId])
            seqCnt += 1
            #sbjSeq += '>%s\n%s\n'%(hitId, dbDict[hitId])
        sbjSeq = r'\n'.join(tmpList)
        querySeq = r'>%s\n%s'%(q, queryDict[q])
        if q.endswith('gene81'):
            print('')
            print(hitId)
            print(memBlastSrc)
            print('')
        #write the command in the bash scripts
        bashFd.write('QUERY="%s"\n'%querySeq)
        bashFd.write('SBJ="%s"\n'%sbjSeq)
        #Execute blast using only one cpu
        blastpCmd = 'blastp -seg no -query <(echo -e $QUERY) -subject <(echo -e $SBJ) -num_alignments %d -matrix %s -dbsize %d -outfmt 5 | %s %d >> %s%s_%s'%(seqCnt, matrix, dbsize, bparser, scoreCoff, outDir, outName, tmpSuffix)
        bashFd.write('%s\n'%blastpCmd)
        blastRunCmd += 1
    os.chmod(memBlastSrc, 0o751)
    if debug:
        print('The bash script %s contains %d blast commands.'%(memBlastSrc, blastRunCmd))
    #execute the bash script
    if debug:
        print('Executing blast commands in\n%s...'%memBlastSrc)
    bashProc = subprocess.Popen(memBlastSrc, shell=True, stdout=subprocess.PIPE, stderr=subprocess.PIPE)
    stdout_val, stderr_val = bashProc.communicate()
    bashProc.wait()
    bashFd.close()
    #return the output file
    return '%s%s_%s'%(outDir, outName, tmpSuffix)



def single_query_qmem_blast_slice_dict_args(paramsDict):
    """
    Execute 2-pass blast for single query sequences on a subset of the hits.

    The input is a dictionary containing all the required parameters for the function to be executed.
    The blast will partially executed in memory but the database files will still be generated
    """
    hitsSlice = paramsDict['hitsSlice']
    hitsDict = paramsDict['hitsDict']
    queryDict = paramsDict['queryDict']
    dbDict = paramsDict['dbDict']
    scoreCoff = paramsDict['scoreCoff']
    outName = paramsDict['outName']
    outDir = paramsDict['outDir']
    tmpSuffix = paramsDict['tmpSuffix']
    debug = paramsDict['debug']
    if debug:
        print('single_query_qmem_blast_slice :: START')
        print('Hits slice size:\t%d'%len(hitsSlice))
        print('Hits dictionary size:\t%d'%len(hitsDict))
        print('Input sequences:\t%s'%len(queryDict))
        print('DB sequences:\t%s'%len(dbDict))
        print('Score cutoff:\t%d'%scoreCoff)
        print('Output name:\t%s'%outName)
        print('Output dir:\t%s'%outDir)
        print('Tmp suffix:\t%s'%tmpSuffix)
    #tmpDir = '%stmp/'%(outDir)
    tmpDir = '%stmp_%s/'%(outDir, tmpSuffix)
    if not os.path.isdir(tmpDir):
        systools.makedir(tmpDir)
    tmpQuery = 'tmpq_%s'%(tmpSuffix)
    tmpDB = 'tmpd_%s'%(tmpSuffix)
    dbsize = 5000000
    bparser = '%sblast_parser.pl'%outDir
    matrix = 'BLOSUM62'
    #####################################
    # example of blastp execution in Memory
    #blastp -query <(echo -e '>sp|P59806|YA177_RHOBA\nTLRFSLENDEVKRLCAATGKSADQIRTEAQAAFAAG') -subject <(echo -e '>sp|P59806|YA177_RHOBA\nMTKTLHFDCLSGISGDMTLGALIDLGVSVDQIQQGLHSLNLPDLKLRTEEVKKCGF')  -outfmt 5 | ./blast_parser.pl <score_cutoff> >> <output_file>
    import subprocess
    #create the bash file with the blast runs
    blastRunCmd = 0
    memBlastSrc = '%smem_blast_%s_%s.sh'%(outDir, outName, tmpSuffix)
    bashFd = open(memBlastSrc, 'w')
    bashFd.write('#!/bin/bash\n')
    for q in hitsSlice:
        dbIds = hitsDict[q]
        blastRunCmd += 1
        #create the small database file
        dbPath = '%s%s_%d'%(tmpDir, tmpDB, blastRunCmd)
        dbSeq = '%s.fasta'%(dbPath)
        #create the small database file
        tmpFd = open(dbSeq, 'w')
        for hitId in dbIds:
            tmpFd.write('>%s\n%s\n'%(hitId, dbDict[hitId]))
        tmpFd.close()
        #create the blast database
        makeDbCmd = 'makeblastdb -in %s -dbtype prot -out %s -title %s'%(dbSeq, dbPath, os.path.basename(dbPath))
        process = subprocess.Popen(makeDbCmd, shell=True, stdout=subprocess.PIPE, stderr=subprocess.PIPE)
        process.wait()
        os.remove(dbSeq)
        #sys.exit('debug')
        #break
        #query sequence
        querySeq = r'>%s\n%s'%(q, queryDict[q])
        '''
        if debug:
            print('Sequences in sbj:\t%d'%seqCnt)
            print('Db seq size (chars):\t%d'%len(sbjSeq))
            #print(sbjSeq)
            print('Query seq size (chars):\t%d'%len(querySeq))
            #print(querySeq)
        '''
        #write the command in the bash scripts
        bashFd.write('QUERY="%s"\n'%querySeq)
        #bashFd.write('DBSEQ="%s"\n'%sbjSeq)
        bashFd.write('DBPATH="%s"\n'%dbPath)
        #Execute blast using only one cpu
        blastpCmd = 'blastp -seg no -query <(echo -e $QUERY) -db $DBPATH -num_alignments %d -matrix %s -dbsize %d -outfmt 5 | %s %d >> %s%s_%s'%(len(dbIds), matrix, dbsize, bparser, scoreCoff, outDir, outName, tmpSuffix)
        bashFd.write('%s\n'%blastpCmd)
    os.chmod(memBlastSrc, 0o751)
    if debug:
        print('The bash script %s contains %d blast commands.'%(memBlastSrc, blastRunCmd))
    #execute the bash script
    if debug:
        print('Executing blast commands in\n%s...'%memBlastSrc)
    bashProc = subprocess.Popen(memBlastSrc, shell=True, stdout=subprocess.PIPE, stderr=subprocess.PIPE)
    stdout_val, stderr_val = bashProc.communicate()
    bashProc.wait()
    bashFd.close()
    #sys.exit('DEBUG')
    #return the output file
    #remove tmp dir and its content
    import shutil
    shutil.rmtree(tmpDir)
    return '%s%s_%s'%(outDir, outName, tmpSuffix)



def single_query_blast_parallel(hitsDict, queryDict, dbDict, scoreCoff=40, dbsize=None, outName=None, outDir=os.getcwd(), outSuffix='chunk', memMode=False, threads=2, debug=False):
    """
    Execute 2-pass blast for single query sequences in parallel.

    The db files will contain all the hits for the single sequence query in the previous blast run
    """
    if debug:
        print('single_query_blast_parallel :: START')
        print('Hits dictionary size:\t%d'%len(hitsDict))
        print('Input sequences:\t%s'%len(queryDict))
        print('DB sequences:\t%s'%len(dbDict))
        print('Score cutoff:\t%d'%scoreCoff)
        print('Output name:\t%s'%outName)
        print('Output dir:\t%s'%outDir)
        print('Threads:\t%d'%threads)
        print('Memory mode:\t%s'%memMode)
        print('Output suffix:\t%s'%outSuffix)
    #slice the keys of the hits dictionary in threads-sezed chunks
    hitsKeys = list(hitsDict)
    sliceSize = int(len(hitsKeys)/threads)
    slices = [hitsKeys[i:i+sliceSize] for i in range(0, len(hitsKeys), sliceSize)]
    if debug:
        print('Number of single query blast for each of the %d threads'%threads)
        [print(len(s)) for s in slices]
    #if there is an extra slice then merge the n slice with n-1
    if len(slices) == threads + 1:
        slices[-2] = slices[-2] + slices[-1]
        #remove the last slice
        slices = slices[:-1]
        if debug:
            print('The list with keys has been updated!')
            [print(len(s)) for s in slices]
    tmpDir = '%stmp/'%(outDir)
    if not os.path.isdir(tmpDir):
        systools.makedir(tmpDir)
    #generate the list of dictionaries that will serve as input parameters
    dictList = []
    for i, s in enumerate(slices):
        tmpDict = OrderedDict()
        tmpDict['hitsSlice'] = slices[i]
        tmpDict['hitsDict'] = hitsDict
        tmpDict['queryDict'] = queryDict
        tmpDict['dbDict'] = dbDict
        tmpDict['scoreCoff'] = scoreCoff
        tmpDict['outName'] = outName
        tmpDict['outDir'] = outDir
        tmpDict['tmpSuffix'] = '%s_%d'%(outSuffix, i + 1)
        tmpDict['debug'] = debug
        dictList.append(tmpDict)
    #sys.exit('DEBUG::single_query_blast_parallel')
    #'''
    from multiprocessing import Pool, Value, Array, Lock, current_process
    pool = Pool(threads)
    if memMode:
        #pool.map(single_query_mem_blast_slice_dict_args, dictList)
        pool.map(single_query_qmem_blast_slice_dict_args, dictList)
    else:
        pool.map(single_query_blast_slice_dict_args, dictList)
    pool.close()
    #'''
    #remove tmp dir and its content
    import shutil
    shutil.rmtree(tmpDir)



def single_query_blastall_parallel(hitsDict, queryDict, dbDict, scoreCoff=40, outName=None, outDir=os.getcwd(), outSuffix='chunk', memMode=False, threads=2, debug=False):
    """
    Execute 2-pass blast for single query sequences in parallel.

    The db files will contain all the hits for the single sequence query in the previous blast run
    """
    if debug:
        print('single_query_blastall_parallel :: START')
        print('Hits dictionary size:\t%d'%len(hitsDict))
        print('Input sequences:\t%s'%len(queryDict))
        print('DB sequences:\t%s'%len(dbDict))
        print('Score cutoff:\t%d'%scoreCoff)
        print('Output name:\t%s'%outName)
        print('Output dir:\t%s'%outDir)
        print('Threads:\t%d'%threads)
        print('Memory mode:\t%s'%memMode)
        print('Output suffix:\t%s'%outSuffix)
    #slice the keys of the hits dictionary in threads-sezed chunks
    hitsKeys = list(hitsDict)
    sliceSize = int(len(hitsKeys)/threads)
    slices = [hitsKeys[i:i+sliceSize] for i in range(0, len(hitsKeys), sliceSize)]
    if debug:
        print('Number of single query blast for each of the %d threads'%threads)
        [print(len(s)) for s in slices]
    #if there is an extra slice then merge the n slice with n-1
    if len(slices) == threads + 1:
        slices[-2] = slices[-2] + slices[-1]
        #remove the last slice
        slices = slices[:-1]
        if debug:
            print('The list with keys has been updated!')
            [print(len(s)) for s in slices]
    tmpDir = '%stmp/'%(outDir)
    if not os.path.isdir(tmpDir):
        systools.makedir(tmpDir)
    #generate the list of dictionaries that will serve as input parameters
    dictList = []
    for i, s in enumerate(slices):
        tmpDict = OrderedDict()
        tmpDict['hitsSlice'] = slices[i]
        tmpDict['hitsDict'] = hitsDict
        tmpDict['queryDict'] = queryDict
        tmpDict['dbDict'] = dbDict
        tmpDict['scoreCoff'] = scoreCoff
        tmpDict['outName'] = outName
        tmpDict['outDir'] = outDir
        tmpDict['tmpSuffix'] = '%s_%d'%(outSuffix, i + 1)
        tmpDict['debug'] = debug
        dictList.append(tmpDict)

    #for d in dictList:
        #single_query_mem_blast_slice_dict_args(d)
        #single_query_qmem_blast_slice_dict_args(d)
        #break

    #sys.exit('DEBUG::single_query_blast_parallel')
    #'''
    from multiprocessing import Pool, Value, Array, Lock, current_process
    pool = Pool(threads)
    if memMode:
        #pool.map(single_query_mem_blast_slice_dict_args, dictList)
        pool.map(single_query_qmem_blast_slice_dict_args, dictList)
    else:
        pool.map(single_query_blastall_slice_dict_args, dictList)
    pool.close()
    #'''
    #remove tmp dir and its content
    import shutil
    shutil.rmtree(tmpDir)



def inparanoid_like_parser(blastOutput, outDir=os.getcwd(), outName=None, scoreCoff=40, plastMode=False, debug=False):
    """Parse tab-separated BLASTP or PLASTP (-outfmt 2) results similarly to how InParanoid does for XML output.

    The resulting file are used by InParanoid to infer orthology
    The blast output fortarm must be obtained using the following parameters:
    -outfmt "6 qseqid sseqid qlen slen qstart qend sstart send bitscore"
    The plastp output must be generated using the parameter "-outfmt 2".
    """
    if debug:
        print('inparanoid_like_parser :: START')
        print('Input blastp result file:%s'%blastOutput)
        print('Outdir:%s'%outDir)
        print('Bit-score cutoff below which hits (the sum of the bit-scores) are discarded:%s'%str(scoreCoff))
        print('Plast mode:\t%s'%plastMode)
    #create the output directory if does not exist yet
    if outDir != os.getcwd():
        systools.makedir(outDir)
    if outDir[-1] != '/':
        outDir += '/'
    #check the existence of the input file
    if not os.path.isfile(blastOutput):
        sys.stderr.write('ERROR: The input file %s was not found, please generate a blastp alignment first.\n'%blastOutput)
        sys.exit(-2)
    #name the output file
    outPath = ''
    if outName is None:
        sys.stderr.write('ERROR: you must specify a name for the output file resulting from the alignments parsing.')
        sys.exit(-5)
    else:
        outPath = '%s%s'%(outDir, outName)
    #dictionaries to store results
    prevHitId = ''
    currentHit = None
    #open the output file
    ofd = open(outPath, 'w')
    #start reading the output file
    rcnt = 0
    wcnt = 0
    for ln in open(blastOutput):
        rcnt += 1
        ln = ln.rstrip()
        if plastMode:
            # query ID, subject ID, percent identities, alignment length, nb. misses,
            # nb. gaps, query begin, query end, subject begin, subject end, e-value,
            # bit score, query length, query frame, query translated, query coverage,
            # nb. gaps in query, subject length, subject frame, subject translated, subject coverage, nb. gaps in subject
            #63363_O67640	63363_O67640	100.00	678	0	0	1	678	1	678	0e+00	1281.2	650 	678	0	0	100.0	0 	678	0	0	100.0	0
            qid, sid, d1, d2, d3, d4, qstart, qend, sstart, send, d5, bitscore, d6, qlen, d7, d8, d9, d10, slen, d11, d12, d13, d14 = ln.split('\t')
            del d1, d2, d3, d4, d5, d6, d7, d8, d9, d10, d11, d12, d13, d14
        else: #blast-tab separated output containing only the following information the following order
            qid, sid, qlen, slen, qstart, qend, sstart, send, bitscore = ln.split('\t')
        #print(ln)
        qlen = int(qlen)
        slen = int(slen)
        qstart = int(qstart)
        qend = int(qend)
        sstart = int(sstart)
        send = int(send)
        bitscore = float(bitscore)
        #if the pair query@subject (hsp) is already in the dictionary
        currentHitId = '%s@%s'%(qid, sid) #normal fasta header should not contain the @ symbol!
        #if not hitId in hitsDict:
        if currentHitId != prevHitId: #then it is a new hit
            if not currentHit is None: #then it is not the first line
                hitScore = None
                outLn = ''
                #finalize score caculation
                prevqid, prevsid = prevHitId.split('@')
                #calculate InParanoid like scores
                hitScore = extract_inparanoid_scores(currentHit, prevqid, prevsid, debug=debug)
                #create the final string if the the bitscore for the hit is higher than the cutoff
                if float(hitScore[2]) >= scoreCoff:
                    for i in range(0, len(hitScore) - 1): #add the first n-1 fields to the output line
                        outLn += '%s\t'%str(hitScore[i])
                    #add the final part
                    outLn += '%s\n'%('\t'.join(hitScore[-1]))
                    ofd.write(outLn)
                    wcnt += 1
            currentHit = OrderedDict() #the ordered dict is better in for calculating the overlaps
            #add the hsp to the dictionary
            currentHit['%d:%d'%(qstart, qend)] = (qlen, slen, qstart, qend, sstart, send, bitscore)
            #initialize the scores
            prevHitId = currentHitId
        else: #hsp for the previous hits
            currentHit['%d:%d'%(qstart, qend)] = (qlen, slen, qstart, qend, sstart, send, bitscore)
    ofd.close()
    if debug:
        print('Total analyzed alignments:\t%d'%rcnt)
        print('Created ortholog relations:\t%d'%wcnt)
    return outPath



def run_inparanoid(inSeq1, inSeq2, inParanoidPackage, outDir=os.getcwd(), threads=4, debug=False):
    """Execute inparanoid for ortholog detection."""
    if debug:
        print('run_inparanoid :: START')
        print('Input seq1:%s'%inSeq1)
        print('Input seq2:%s'%inSeq2)
        print('Inparanoid package:%s'%inParanoidPackage)
        print('Outdir:%s'%outDir)
        print('Cpus (for blastp):\t%d'%threads)
    #check the existence of the input file
    if not os.path.isfile(inSeq1):
        sys.stderr.write('ERROR: The first input file %s was not found, please provide the path to a valid file.\n'%inSeq1)
        sys.exit(-2)
    if not os.path.isfile(inSeq2):
        sys.stderr.write('ERROR: The second input file %s was not found, please provide the path to a valid file.\n'%inSeq2)
        sys.exit(-2)
    #check the existance of the zip package
    if inParanoidPackage is None:
        sys.stderr.write('\nERROR: You must specify a package contanining inparanoid programs.\n')
        sys.exit(-3)
    if not os.path.isfile(inParanoidPackage):
        sys.stderr.write('\nERROR: The archive file %s which should contain inparanoid scripts was not found, please provide the path to a valid file'%inParanoidPackage)
        sys.exit(-2)
    #create the output directory if does not exist yet
    if outDir != os.getcwd():
        if not os.path.isdir(outDir):
            systools.makedir(outDir)
    if outDir[-1] != '/':
        outDir += '/'
    #check the output directory differs from the directrory in which the input files are
    if os.path.dirname(inSeq1) + '/' == outDir:
        sys.stderr.write('ERROR: the output directory %s\nmust be different from the one in which the input files are stored.')
        sys.exit(-2)
    if os.path.dirname(inSeq2) + '/' == outDir:
        sys.stderr.write('ERROR: the output directory %s\nmust be different from the one in which the input files are stored.')
        sys.exit(-2)
    #copy the input files to the output directory
    tmpIn1 = '%s%s'%(outDir, os.path.basename(inSeq1))
    tmpIn2 = '%s%s'%(outDir, os.path.basename(inSeq2))
    systools.copy(inSeq1, tmpIn1, metaData=True, debug=debug)
    systools.copy(inSeq2, tmpIn2, metaData=True, debug=debug)
    #untar the inparanoid archive
    systools.untar(inParanoidPackage, outDir, debug=debug)
    #create the output file name
    outName1 = os.path.basename(tmpIn1)
    outName2 = os.path.basename(tmpIn2)
    outAA = '%s-%s'%(outName1, outName1)
    outAB = '%s-%s'%(outName1, outName2)
    outBA = '%s-%s'%(outName2, outName1)
    outBB = '%s-%s'%(outName2, outName2)
    outTable = 'table.%s-%s'%(outName1, outName2)
    outSql = 'sqltable.%s-%s'%(outName1, outName2)
    outSummary = 'Output.%s-%s'%(outName1, outName2)
    #inparanoid path
    inparanoid = '%sinparanoid.pl'%outDir
    os.chmod(inparanoid, 0o751)
    blastparser = '%sblast_parser.pl'%outDir
    os.chmod(blastparser, 0o751)
    #change the current working directory
    prevCwd = os.getcwd()
    os.chdir(outDir)
    #create the command for the prediction
    #EXAMPLE:
    #inparanoid.pl seq1.fasta seq2.fasta
    cmd = '%s %s %s'%(inparanoid, os.path.basename(tmpIn1), os.path.basename(tmpIn2))
    if debug:
        print('\nINPARANOID CMD:\n%s'%cmd)
    #execute the system call
    process = subprocess.Popen(cmd, shell=True, stdout=subprocess.PIPE, stderr=subprocess.PIPE)
    stdout_val, stderr_val = process.communicate() #get stdout and stderr
    process.wait()
    debug = False
    if debug:
        print('\nSTDOUT:\n%s'%repr(stdout_val))
        print('\nSTDERR:\n%s'%repr(stderr_val))
    #write the log file
    logPath = '%sinparanoid.log'%outDir
    ofd = open(logPath, 'w')
    ofd.write(stdout_val.decode())
    ofd.write(stderr_val.decode())
    ofd.close()
    #check that all the files have been created
    if not os.path.isfile(outTable):
        sys.stderr.write('WARNING: the inparanoid output table file %s was not generated.')
        return(None, None, None, False)
    if not os.path.isfile(outSql):
        sys.stderr.write('WARNING: the inparanoid sql table %s was not generated.')
        return(None, None, None, False)
    if not os.path.isfile(outSummary):
        sys.stderr.write('WARNING: the inparanoid summary output %s was not generated.')
        return(None, None, None, False)
    #calcalute the execution time
    tot_time, execTimePath = calc_inparanoid_exec_time(outDir, outDir=outDir, debug=debug)
    #everything went ok!
    os.chdir(prevCwd)
    #return the output paths
    return(outTable, outSql, outSummary, tot_time, True)



def run_inparanoid_parallel(inSeq1, inSeq2, inParanoidPackage, outDir=os.getcwd(), threads=4, sharedDir=None, reuse=True, noBlast=False, memMode=False, bootstrap=False, debug=False):
    """Execute inparanoid, running blast in parallel if required."""
    if debug:
        print('run_inparanoid_parallel :: START')
        print('Input seq1:%s'%inSeq1)
        print('Input seq2:%s'%inSeq2)
        print('Inparanoid package:%s'%inParanoidPackage)
        print('Outdir:%s'%outDir)
        print('Cpus (for blastp):\t%d'%threads)
        print('Shared output directory:\t%s'%sharedDir)
        print('Reuse mode:\t%s'%reuse)
        print('No blast:\t%s'%noBlast)
        print('Bootstrap:\t%s'%bootstrap)
        print('Memory mode:\t%s'%memMode)
    #check the existence of the input file
    if not os.path.isfile(inSeq1):
        sys.stderr.write('ERROR: The first input file %s was not found, please provide the path to a valid file.\n'%inSeq1)
        sys.exit(-2)
    if not os.path.isfile(inSeq2):
        sys.stderr.write('ERROR: The second input file %s was not found, please provide the path to a valid file.\n'%inSeq2)
        sys.exit(-2)
    #check the existance of the zip package
    if inParanoidPackage is None:
        sys.stderr.write('\nERROR: You must specify a package contanining inparanoid programs.\n')
        sys.exit(-3)
    if not os.path.isfile(inParanoidPackage):
        sys.stderr.write('\nERROR: The archive file %s which should contain inparanoid scripts was not found, please provide the path to a valid file.\n'%inParanoidPackage)
        sys.exit(-2)
    #create the output directory if does not exist yet
    if outDir != os.getcwd():
        if not os.path.isdir(outDir):
            systools.makedir(outDir)
    if outDir[-1] != '/':
        outDir += '/'
    #check the existance if the shared directory
    if sharedDir is not None:
        if not os.path.isdir(sharedDir):
            sys.stderr.write('\nWARNING: the path to shared directory %s is not valid, a new directory will be created\n'%sharedDir)
            systools.makedir(sharedDir)
    else: #create it inside the output directory
        sharedDir = '%sshared_output/'%outDir
        systools.makedir(sharedDir)
        if debug:
            print('Shared directory created in %s'%sharedDir)
    #check the output directory differs from the directory in which the input files are
    if os.path.dirname(inSeq1) + '/' == outDir:
        sys.stderr.write('\nERROR: the output directory %s\nmust be different from the one in which the input files are stored.\n')
        sys.exit(-2)
    if os.path.dirname(inSeq2) + '/' == outDir:
        sys.stderr.write('\nERROR: the output directory %s\nmust be different from the one in which the input files are stored.\n')
        sys.exit(-2)
    #copy the input files to the output directory
    tmpIn1 = '%s%s'%(outDir, os.path.basename(inSeq1))
    tmpIn2 = '%s%s'%(outDir, os.path.basename(inSeq2))
    systools.copy(inSeq1, tmpIn1, metaData=True, debug=debug)
    systools.copy(inSeq2, tmpIn2, metaData=True, debug=debug)
    #untar the inparanoid archive
    systools.untar(inParanoidPackage, outDir, debug=debug)
    #create the output file name
    outName1 = os.path.basename(tmpIn1)
    outName2 = os.path.basename(tmpIn2)
    outAA = '%s-%s'%(outName1, outName1)
    outAB = '%s-%s'%(outName1, outName2)
    outBA = '%s-%s'%(outName2, outName1)
    outBB = '%s-%s'%(outName2, outName2)
    outTable = 'table.%s-%s'%(outName1, outName2)
    outSql = 'sqltable.%s-%s'%(outName1, outName2)
    outSummary = 'Output.%s-%s'%(outName1, outName2)
    executionTime = 'execution_time.%s-%s'%(outName1, outName2)
    #inparanoid path
    inparanoid = '%sinparanoid.pl'%outDir
    os.chmod(inparanoid, 0o751)
    blastparser = '%sblast_parser.pl'%outDir
    os.chmod(blastparser, 0o751)
    #change the current working directory
    prevCwd = os.getcwd()
    os.chdir(outDir)
    #start the timing
    start_time = time.time()
    ##### Execute the required blast runs ######
    in1Dict = load_input_sequences(tmpIn1, debug=debug)
    in2Dict = load_input_sequences(tmpIn2, debug=debug)
    #set the execution time variables
    outPathAA = outPathBB = outPathBA = outPathAB = None
    tot_blast_time_AA = blast1_time_AA = blast2_time_AA = 0
    tot_blast_time_BB = blast1_time_BB = blast2_time_BB = 0
    blast1_time_AB = blast2_time_AB = tot_blast_time_AB = 0
    blast1_time_BA = blast2_time_BA = tot_blast_time_BA = 0
    #Execute the blast if required
    if not noBlast:
        #compare AA
        if reuse:
            oldAA = '%s%s'%(sharedDir, outAA)
            if os.path.isfile(oldAA):
                systools.copy(oldAA, outDir, metaData=True)
                if debug:
                    sys.stdout.write('\nSuccessfully copied inparanoid output for\n%s\nto the inparanoid run directory\n%s\n'%(oldAA, outDir))
                    sys.stdout.write('The blast run for %s can be skipped!\n'%(outAA))
                tot_blast_time_AA = blast1_time_AA = blast2_time_AA = 0
                outPathAA = '%s%s'%(outDir, outAA)
            else: #the run is required
                outPathAA, blast1_time_AA, blast2_time_AA, tot_blast_time_AA = blast_2pass(tmpIn1, tmpIn1, in1Dict, in1Dict, scoreCoff=40, outName=outAA, outDir=outDir, threads=threads, memMode=memMode, debug=debug)
                if not os.path.isfile('%s%s'%(sharedDir, os.path.basename(outPathAA))):
                    #copy the generated file to the shared directory
                    systools.copy(outPathAA, sharedDir,  metaData=True)
        else: #just compare the 2
            outPathAA, blast1_time_AA, blast2_time_AA, tot_blast_time_AA = blast_2pass(tmpIn1, tmpIn1, in1Dict, in1Dict, scoreCoff=40, outName=outAA, outDir=outDir, threads=threads, memMode=memMode, debug=debug)
        #compare AB
        outPathAB, blast1_time_AB, blast2_time_AB, tot_blast_time_AB = blast_2pass(tmpIn1, tmpIn2, in1Dict, in2Dict, scoreCoff=40, outName=outAB, outDir=outDir, threads=threads, memMode=memMode, debug=debug)
        #compare BA
        outPathBA, blast1_time_BA, blast2_time_BA, tot_blast_time_BA = blast_2pass(tmpIn2, tmpIn1, in2Dict, in1Dict, scoreCoff=40, outName=outBA, outDir=outDir, threads=threads, memMode=memMode, debug=debug)
        #compare BB
        if reuse:
            oldBB = '%s%s'%(sharedDir, outBB)
            if os.path.isfile(oldBB):
                systools.copy(oldBB, outDir)
                if debug:
                    sys.stdout.write('\nSuccessfully copied inparanoid output for\n%s\nto the inparanoid run directory\n%s\n'%(oldBB, outDir))
                    sys.stdout.write('The blast run for %s can be skipped!\n'%(outBB))
                tot_blast_time_BB = blast1_time_BB = blast2_time_BB = 0
                outPathBB = '%s%s'%(outDir, outBB)
            else: #the run is required
                outPathBB, blast1_time_BB, blast2_time_BB, tot_blast_time_BB = blast_2pass(tmpIn2, tmpIn2, in2Dict, in2Dict, scoreCoff=40, outName=outBB, outDir=outDir, threads=threads, memMode=memMode, debug=debug)
                #copy the generated to the shared directory
                systools.copy(outPathBB, sharedDir,  metaData=True)
        else: #just compare the 2
            outPathBB, blast1_time_BB, blast2_time_BB, tot_blast_time_BB = blast_2pass(tmpIn2, tmpIn2, in2Dict, in2Dict, scoreCoff=40, outName=outBB, outDir=outDir, threads=threads, memMode=memMode, debug=debug)
    #check that the output tables for AA, AB, BA and BB do exist
    if outPathAA is None:
        outPathAA = '%s%s'%(outDir, outAA)
    if not os.path.isfile(outPathAA):
        sys.stderr.write('ERROR: the first inparnoid table for AA (%s-%s) is missing, please create it using blast.\n'%(outName1, outName1))
        sys.exit(-2)
    if outPathBB is None:
        outPathBB = '%s%s'%(outDir, outBB)
    if not os.path.isfile(outPathBB):
        sys.stderr.write('ERROR: the first inparnoid table for BB (%s-%s) is missing, please create it using blast.\n'%(outName2, outName2))
        sys.exit(-2)
    if outPathAB is None:
        outPathAB = '%s%s'%(outDir, outAB)
    if not os.path.isfile(outPathAB):
        sys.stderr.write('ERROR: the first inparnoid table for AB (%s-%s) is missing, please create it using blast.\n'%(outName1, outName2))
        sys.exit(-2)
    if outPathBA is None:
        outPathBA = '%s%s'%(outDir, outBA)
    if not os.path.isfile(outPathBA):
        sys.stderr.write('ERROR: the first inparnoid table for BA (%s-%s) is missing, please create it using blast.\n'%(outName2, outName1))
        sys.exit(-2)
    #create the command for the prediction
    #EXAMPLE: inparanoid.pl seq1.fasta seq2.fasta
    inpara_start = time.time()
    #cmd = '%s %s %s'%(inparanoid, os.path.basename(tmpIn1), os.path.basename(tmpIn2))
    cmd = '%s %s %s %d %d'%(inparanoid, os.path.basename(tmpIn1), os.path.basename(tmpIn2), threads, int(bootstrap))
    if debug:
        print('\nINPARANOID CMD:\n%s'%cmd)
    #execute the system call
    process = subprocess.Popen(cmd, shell=True, stdout=subprocess.PIPE, stderr=subprocess.PIPE)
    stdout_val, stderr_val = process.communicate() #get stdout and stderr
    process.wait()
    #write the log file
    logPath = '%sinparanoid.log'%outDir
    ofd = open(logPath, 'w')
    ofd.write(stdout_val.decode())
    ofd.write(stderr_val.decode())
    ofd.close()
    #check that all the files have been created
    if not os.path.isfile(outTable):
        sys.stderr.write('WARNING: the inparanoid output table file %s was not generated.')
        return(None, None, None, False)
    if not os.path.isfile(outSql):
        sys.stderr.write('WARNING: the inparanoid sql table %s was not generated.')
        return(None, None, None, False)
    if not os.path.isfile(outSummary):
        sys.stderr.write('WARNING: the inparanoid summary output %s was not generated.')
        return(None, None, None, False)
    #everything went ok!
    end_time = time.time()
    inpara_tot = round(end_time - inpara_start, 2)
    sys.stdout.write('\nInparanoid elapsed time (seconds):\t%s\n'%str(inpara_tot))
    tot_time = round(end_time - start_time, 2)
    sys.stdout.write('\nTotal elapsed time (seconds):\t%s\n'%str(tot_time))
    #write the file with execution time
    executionTimePath = '%s%s'%(outDir, executionTime)
    ofd = open(executionTimePath, 'w')
    ofd.write('input1\tinput2\tinput1_size\tinput1_size\tblastAA_pass1\tblastAA_pass2\tblastAA_tot\tblastAB_pass1\tblastAB_pass2\tblastAB_tot\tblastBA_pass1\tblastBA_pass2\tblastBA_tot\tblastBB_pass1\tblastBB_pass2\tblastBB_tot\tinparanoid\ttotal_runtime\n')
    #now write the values
    ofd.write('%s\t%s\t%d\t%d\t%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s\n'%(os.path.basename(tmpIn1), os.path.basename(tmpIn2), len(in1Dict), len(in2Dict), blast1_time_AA, blast2_time_AA, tot_blast_time_AA, blast1_time_AB, blast2_time_AB, tot_blast_time_AB, blast1_time_BA, blast2_time_BA, tot_blast_time_BA, blast1_time_BB, blast2_time_BB, tot_blast_time_BB, inpara_tot, tot_time))
    ofd.close()
    os.chdir(prevCwd)
    #cleanup! remove input files and blast database files
    #copied input
    if os.path.isfile(tmpIn1):
        os.remove(tmpIn1)
    if os.path.isfile(tmpIn2):
        os.remove(tmpIn2)
    #db files
    tmpDbPath = '%s.psq'%tmpIn1
    if os.path.isfile(tmpDbPath):
        os.remove(tmpDbPath)
    tmpDbPath = '%s.pin'%tmpIn1
    if os.path.isfile(tmpDbPath):
        os.remove(tmpDbPath)
    tmpDbPath = '%s.phr'%tmpIn1
    if os.path.isfile(tmpDbPath):
        os.remove(tmpDbPath)
    tmpDbPath = '%s.psq'%tmpIn2
    if os.path.isfile(tmpDbPath):
        os.remove(tmpDbPath)
    tmpDbPath = '%s.pin'%tmpIn2
    if os.path.isfile(tmpDbPath):
        os.remove(tmpDbPath)
    tmpDbPath = '%s.phr'%tmpIn2
    if os.path.isfile(tmpDbPath):
        os.remove(tmpDbPath)
    #return the output paths
    return(outTable, outSql, outSummary, tot_time, True)



def run_pyparanoid(inDir, inParanoidPackage, outDir=os.getcwd(), threads=4, sharedDir=None, reuse=True, noBlast=False, memMode=False, bootstrap=False, overwrite=False, debug=False):
    """Execute inparanoid, running blast in parallel if required for all the species in the input directory."""
    import platform
    ostype = platform.system()
    if debug:
        print('run_pyparanoid :: START')
        print('Input directory:%s'%inDir)
        print('Inparanoid package:%s'%inParanoidPackage)
        print('Output directory:%s'%outDir)
        print('Cpus (for blastp):\t%d'%threads)
        print('Shared output directory:\t%s'%sharedDir)
        print('Reuse mode:\t%s'%reuse)
        print('Skip blast:\t%s'%noBlast)
        print('Bootstrap:\t%s'%bootstrap)
        print('Overwrite existing runs:\t%s'%overwrite)
        print('Memory mode:\t%s'%memMode)
    #check the existence of the input directory
    if inDir[-1] != '/':
        inDir += '/'
    if not os.path.isdir(inDir):
        sys.stderr.write('ERROR: the directory with the input file %s was not found, please provide the path to a directory.\n'%inDir)
        sys.exit(-2)
    #check that the input directory is not empty
    tmpFlist = os.listdir(inDir)
    if ostype == 'Darwin':
        if '.DS_Store' in tmpFlist:
            tmpFlist.remove('.DS_Store')
    if len(tmpFlist) < 3:
        sys.stderr.write('ERROR: the directory with the input files only contains %d (%s) files\n Please provide at least 3 genomes.\n'%(len(tmpFlist), ', '.join(tmpFlist)))
        sys.exit(-2)
    #make sure that all the file do not contain file extensions nor '-' in their names
    for f in tmpFlist:
        if '-' in f:
            sys.stderr.write('ERROR: the file names including the \'-\' symbol (%s) are not allowed.\n Please remove the \'-\' from the species names.\n'%(f))
            sys.exit(-2)
        if '.' in f:
            sys.stderr.write('ERROR: the file (%s) includes the \'.\' symbol. This might reduce the readability of the output file names.\nPlease remove \'.\' from your input species names.\n'%(f))
            sys.exit(-2)
    #create the output directory if does not exist yet
    if outDir != os.getcwd():
        if not os.path.isdir(outDir):
            systools.makedir(outDir)
    if outDir[-1] != '/':
        outDir += '/'
    #check the existance if the shared directory
    if sharedDir is not None:
        if not os.path.isdir(sharedDir):
            sys.stderr.write('\nWARNING: the path to shared directory %s is not valid, a new directory will be created\n'%sharedDir)
            systools.makedir(sharedDir)
    else: #create it inside the output directory
        sharedDir = '%sshared_output/'%outDir
        systools.makedir(sharedDir)
        if debug:
            print('Shared directory created in %s'%sharedDir)
    #directories that will contain the within and between proteome comparisons
    wpDir = '%swithin_proteomes/'%sharedDir#contain sdirectories with within-proteome comparisons
    systools.makedir(wpDir)
    btwDir = '%sbetween_proteomes/'%sharedDir#contain sdirectories with within-proteome comparisons
    systools.makedir(btwDir)
    #sort the list with the input files (to avoid direfferent behaviours in OSX and Linux)
    tmpFlist.sort()
    #create the file with species names
    spFile = '%sspecies.txt'%(outDir)
    spList = []
    ofd = open(spFile, 'w')
    for f in tmpFlist:
        ofd.write('%s\n'%f)
        spList.append(f)
    ofd.close()
    del tmpFlist
    #generate file with the combinations
    spPairsFile = '%sspecies_pairs.txt'%(outDir)
    spPairs = list(itertools.combinations(spList, r=2))
    spPairs.sort()
    #check that the file with genome pairs has not been created yet
    if os.path.isfile(spPairsFile):
        sys.stderr.write('WARNING: a file (%s) with the species combination already exists and will not be recreated.\n'%spPairsFile)
    else:
        ofd = open(spPairsFile, 'w')
        [ofd.write('%s-%s\n'%(tpl[0], tpl[1])) for tpl in spPairs]
        ofd.close()
    #give some information about the combinations
    dashedPairs = ['%s-%s'%(tpl[0], tpl[1]) for tpl in spPairs]
    #list with the within-proteome pairs (eg., sp1-sp1)
    wpPairs = ['%s-%s'%(el, el) for el in spList]
    print('For the %d input species %d combinations are possible.'%(len(spList), len(spPairs)))
    #check that for directories with completed runs
    tmpFlist = os.listdir(outDir)
    #remove the .DS_STORE if needed
    if ostype == 'Darwin':
        for name in tmpFlist:
            if name[0] == '.':
                tmpFlist.remove(name)
    #sort the list with pairs
    tmpFlist.sort()
    #sys.exit('DEBUG :: pyparanoid')
    skipList = []
    requiredPairs = []
    if not overwrite: #populate the list with the runs to be skipped if required
        for name in tmpFlist:
            #print(name)
            if name in dashedPairs:
                skipList.append(name)
        #populate the list with required runs
        for pair in dashedPairs:
            if not pair in skipList:
                requiredPairs.append(pair)
    else:
        requiredPairs = dashedPairs
    if debug:
        print('dashedPairs ::\t%s'%(dashedPairs))
        print('requiredPairs ::\t%s'%(requiredPairs))
        print('skipList ::\t%s'%(skipList))
    #generate within species pairs

    #write some info about the required runs
    if (len(skipList) > 0) and (not overwrite):
        print('Comparisons for %d species pairs are already avaliable and will be skipped.'%len(skipList))
    print('The ortholog relashionships will be searched for the following %d proteome pairs:\n%s\n'%(len(spPairs) - len(skipList), ', '.join(requiredPairs)))
    #sys.exit('DEBUG :: pyparanoid')
    #lets start the inparanoid runs
    for pair in requiredPairs:
        f1, f2 = pair.split('-')
        f1Path = '%s%s'%(inDir, f1)
        f2Path = '%s%s'%(inDir, f2)
        pairOutDir = '%s%s/'%(outDir, pair)
        #run only if the directory does not exists, or the overwrite flag is true
        if not os.path.isdir(pairOutDir) or overwrite:
            print('\nFind orthologs for the pair %s'%pair)
            tbl, sql, summary, exec_time, success = run_inparanoid_parallel(f1Path, f2Path, inParanoidPackage=inParanoidPackage, outDir=pairOutDir, threads=threads, sharedDir=sharedDir, reuse=True, noBlast=noBlast, memMode=memMode, bootstrap=bootstrap, debug=debug)
    return (spFile, spPairsFile)



def run_quickparanoid(sqlTblDir=os.getcwd(), outDir=os.getcwd(), srcDir=None, outName=None, speciesFile=None, maxGenePerSp=10, debug=False):
    """Prepare configuration file for quickparanoid and execute it."""
    if debug:
        print('run_quickparanoid :: START')
        print('Input SQL tables directory:\t%s'%sqlTblDir)
        print('QuickParanoid output directory:\t%s'%outDir)
        print('Directory with binaries for quick MultiParanoid:\t%s'%srcDir)
        print('Output cluster name:\t%s'%outName)
        print('Species names:\t%s'%speciesFile)
    if srcDir is None:
        sys.stderr.write('ERROR: you must provide the path to the directory containing quick multiparanoid files\n')
        sys.exit(-5)
    #check that the input directory is valid
    if not os.path.isdir(sqlTblDir):
        sys.stderr.write('ERROR: the directory containing the ortholog tables \n%s\n does not exist.\n'%sqlTblDir)
        sys.exit(-2)
    if not os.path.isfile(speciesFile):
        sys.stderr.write('ERROR: you must provide a file containing all the species names\n')
        sys.exit(-2)
    #load the species names
    species = []
    for ln in open(speciesFile):
        species.append(ln.rstrip())
    #check that the species list not empty
    if len(species) < 2:
        sys.stderr.write('ERROR: the list with species names must contain at least 2 species names.\n')
        sys.exit(-4)
    #create the output directory if does not exist yet
    if outDir[-1] != '/':
        outDir += '/'
    systools.makedir(outDir)
    # copy the files
    copy_quickparanoid_files(srcDir=srcDir, outDir=outDir, debug=debug)
    #change the mode foe the main executable file qp, qa1 and qa2
    qp = '%sqp'%outDir
    config = '%sconfig'%outDir
    #write the species names in the config file
    ofd = open(config, 'w')
    for el in species:
        ofd.write('%s\n'%el)
    ofd.close()
    #enter the root directory
    prevDir = os.getcwd()
    os.chdir(outDir)
    # Run quickparanoid
    # EXAMPLE: ./qp
    print('\nCreating multi-species ortholog groups...')
    process = subprocess.Popen(qp, shell=True, stdout=subprocess.PIPE, stderr=subprocess.PIPE, stdin=subprocess.PIPE)
    stdout_val, stderr_val = process.communicate() #get stdout and stderr
    if debug:
        if stdout_val is not None:
            print('\nQuickparanoid compile script STDOUT:\n%s'%str(stdout_val.decode()))
        if stderr_val is not None:
            print('\nQuickparanoid compile script STDERR:\n%s'%str(stderr_val.decode()))
    process.wait()
    #now generate the clusters
    qpBin = '%stest'%outDir
    outClstrPath = outDir
    if outName is None:
        outName = 'multispecies_clusters.tsv'
    outClstrPath = '%s%s'%(outDir, outName)
    cmd = '%s'%(qpBin)
    if debug:
        print(cmd)
    process = subprocess.Popen(cmd, shell=True, stdout=subprocess.PIPE, stderr=subprocess.PIPE)
    stdout_val, stderr_val = process.communicate() #get stdout and stderr
    # open the output file and write the clusters in it
    tmpOfd = open(outClstrPath, 'w')
    tmpOfd.write(stdout_val.decode())
    tmpOfd.close()
    if debug:
        if stdout_val is not None:
            print('\nQuickparanoid clustering STDOUT:\n%s'%str(stdout_val.decode()))
        if stderr_val is not None:
            print('\nQuickparanoid clustering STDERR:\n%s'%str(stderr_val.decode()))
    process.wait()
    #fix the hdr in quickparanoid output
    tmpClstr = '%stmp_cstrs.txt'%outDir
    ofd = open(tmpClstr, 'w')
    for ln in open(outClstrPath):
        if ln[0] == '#': # this is the hdr
            ln = ln.rstrip()
            ofd.write('%s\n'%ln[1:])
        else:
            ofd.write(ln)
    ofd.close()
    systools.move(tmpClstr, outClstrPath, debug=debug)
    #now count the cluster numbers
    clstrStats = '%sstats_%s'%(outDir, os.path.basename(outClstrPath))
    totClstr, noConflict, diffNameCnt, diffNumCnt = count_clusters_no_pandas(outClstrPath, debug=debug)
    ofd = open(clstrStats, 'w')
    ofd.write('clstrs_cnt\tno_conflict_cnt\tdiff_names_cnt\tdiff_numbers_cnt\ttot_conflicts\n')
    ofd.write('%d\t%d\t%d\t%d\t%d\n'%(totClstr, noConflict, diffNameCnt, diffNumCnt, diffNameCnt + diffNumCnt))
    ofd.close()
    #remove the sql tables, and other not required files
    reduntand_files = ['qa1', 'qa2','qp', 'dump', 'Makefile', 'Makefile.in']
    reduntand_files.append('test')
    reduntand_files.append('tests')
    reduntand_files.append('gen_header')
    for fname in os.listdir(outDir):
        if fname.startswith('sqltable.') or fname in reduntand_files:
            os.remove(os.path.join(outDir, fname))
    # reset working directory
    os.chdir(prevDir)
    # prettify the output
    prettyOutPath = prettify_multispecies_output(outClstrPath, outDir=outDir, refSpeciesList=species, minScore=0.05, maxGenePerSp=maxGenePerSp, debug=debug)
    # remove the old file and rename the pretty output
    os.remove(outClstrPath)
    shutil.move(prettyOutPath, outClstrPath)
    return outClstrPath



def run_sonicparanoid2(inDir, outDir=os.getcwd(), threads=4, sharedDir=None, mmseqsDbDir=None, sensitivity=4.0, cutoff=40, confCutoff=0.05, lenDiffThr=0.5, reuse=True, noMmseqs=False, overwrite=False, debug=False):
    """Execute sonicparanoid, using MMseqs2 if required for all the proteomes in the input directory."""
    import platform
    ostype = platform.system()
    if debug:
        print('\nrun_sonicparanoid2 :: START')
        print('Input directory:%s'%inDir)
        print('Output directory:%s'%outDir)
        print('CPUs:\t%d'%threads)
        print('Shared output directory:\t%s'%sharedDir)
        print('MMseqs2 database directory:\t%s'%mmseqsDbDir)
        print('MMseqs2 sensitivity (-s):\t%s'%str(sensitivity))
        print('Cutoff:\t%d'%cutoff)
        print('Confidence cutoff for paralogs:\t%s'%str(confCutoff))
        print('Length difference filtering threshold:\t%s'%str(lenDiffThr))
        print('Reuse mode:\t%s'%reuse)
        print('Skip MMseqs2 alignments:\t%s'%noMmseqs)
        print('Overwrite existing runs:\t%s'%overwrite)
    #check the existence of the input directory
    if inDir[-1] != '/':
        inDir += '/'
    if not os.path.isdir(inDir):
        sys.stderr.write('ERROR: the directory with the input file %s was not found, please provide the path to a directory.\n'%inDir)
        sys.exit(-2)
    #check that the input directory is not empty
    tmpFlist = os.listdir(inDir)
    if ostype == 'Darwin':
        if '.DS_Store' in tmpFlist:
            tmpFlist.remove('.DS_Store')
    if len(tmpFlist) < 3:
        sys.stderr.write('ERROR: the directory with the input files only contains %d (%s) files\n Please provide at least 3 genomes.\n'%(len(tmpFlist), ', '.join(tmpFlist)))
        sys.exit(-2)
    #make sure that all the file do not contain file extensions nor '-' in their names
    for f in tmpFlist:
        if '-' in f:
            sys.stderr.write('ERROR: the file names including the \'-\' symbol (%s) are not allowed.\n Please remove the \'-\' from the species names.\n'%(f))
            sys.exit(-5)
        if '.' in f:
            sys.stderr.write('ERROR: the file (%s) includes the \'.\' symbol. This might reduce the readability of the output file names.\nPlease remove \'.\' from your input species names.\n'%(f))
            sys.exit(-5)
    #create the output directory if does not exist yet
    systools.makedir(outDir)
    if outDir[-1] != '/':
        outDir += '/'
    #check the existance if the shared directory
    if sharedDir is not None:
        if not os.path.isdir(sharedDir):
            sys.stderr.write('\nWARNING: the path to shared directory %s is not valid, a new directory will be created\n'%sharedDir)
            systools.makedir(sharedDir)
    else: #create it inside the output directory
        sharedDir = '%sshared_output/'%outDir
        systools.makedir(sharedDir)
        if debug:
            print('Shared directory created in %s'%sharedDir)
    #check cutoff and woed size
    if cutoff < 30:
        cutoff = 40
    #sort the list with the input files (to avoid direfferent behaviours in OSX and Linux)
    tmpFlist.sort()
    #create the file with species names
    spFile = '%sspecies.txt'%(outDir)
    spList = []
    ofd = open(spFile, 'w')
    for f in tmpFlist:
        ofd.write('%s\n'%f)
        spList.append(f)
    ofd.close()
    del tmpFlist
    #generate file with the combinations
    spPairsFile = '%sspecies_pairs.txt'%(outDir)
    spPairs = list(itertools.combinations(spList, r=2))
    spPairs.sort()
    #check that the file with genome pairs has not been created yet
    if os.path.isfile(spPairsFile):
        sys.stderr.write('WARNING: a file (%s) with the species combination already exists and will not be recreated.\n'%spPairsFile)
    else:
        ofd = open(spPairsFile, 'w')
        [ofd.write('%s-%s\n'%(tpl[0], tpl[1])) for tpl in spPairs]
        ofd.close()
    #give some information about the combinations
    dashedPairs = ['%s-%s'%(tpl[0], tpl[1]) for tpl in spPairs]
    print('For the %d input species %d combinations are possible.'%(len(spList), len(spPairs)))
    #check that for directories with completed runs
    tmpFlist = os.listdir(outDir)
    #remove the .DS_STORE if needed
    if ostype == 'Darwin':
        for name in tmpFlist:
            if name[0] == '.':
                tmpFlist.remove(name)
    #sort the list with pairs
    tmpFlist.sort()
    skipList = []
    requiredPairs = []
    if not overwrite: #populate the list with the runs to be skipped if required
        for name in tmpFlist:
            #print(name)
            if name in dashedPairs:
                skipList.append(name)
        #populate the list with required runs
        for pair in dashedPairs:
            if not pair in skipList:
                requiredPairs.append(pair)
    else:
        requiredPairs = dashedPairs
    if debug:
        print('dashedPairs ::\t%s'%(dashedPairs))
        print('requiredPairs ::\t%s'%(requiredPairs))
        print('skipList ::\t%s'%(skipList))
    #write some info about the required runs
    if (len(skipList) > 0) and (not overwrite):
        print('Comparisons for %d species pairs are already avaliable and will be skipped.'%len(skipList))
    print('The ortholog relashionships will be searched for the following %d proteome pairs:\n%s\n'%(len(spPairs) - len(skipList), ', '.join(requiredPairs)))
    #lets start the inparanoid runs
    for pair in requiredPairs:
        f1, f2 = pair.split('-')
        f1Path = '%s%s'%(inDir, f1)
        f2Path = '%s%s'%(inDir, f2)
        pairOutDir = '%s%s/'%(outDir, pair)
        #run only if the directory does not exists, or the overwrite flag is true
        if not os.path.isdir(pairOutDir) or overwrite:
            tbl, sql, summary, exec_time, success = run_sonicparanoid2_parallel_mmseqs(f1Path, f2Path, outDir=pairOutDir, threads=threads, sharedDir=sharedDir, mmseqsDbDir=mmseqsDbDir, sensitivity=sensitivity, cutoff=cutoff, confCutoff=confCutoff, lenDiffThr=lenDiffThr,  reuse=True, noMmseqs=noMmseqs, debug=debug)
        #sys.exit('DEBUG :: run_sonicparanoid2')
    return (spFile, spPairsFile)


def run_sonicparanoid2_multiproc(inDir, outDir=os.getcwd(), threads=4, sharedDir=None, mmseqsDbDir=None, create_idx=True, sensitivity=4.0, cutoff=40, confCutoff=0.05, lenDiffThr=0.5, overwrite_all=False, overwrite_tbls=False, update_run=False, keepAlign=False, debug=False):
    """Execute sonicparanoid, using MMseqs2 if required for all the proteomes in the input directory."""
    import platform
    import copy
    ostype = platform.system()
    if debug:
        print('\nrun_sonicparanoid2_multiproc :: START')
        print('Input directory:%s'%inDir)
        print('Output directory:%s'%outDir)
        print('CPUs:\t%d'%threads)
        print('Shared output directory:\t%s'%sharedDir)
        print('MMseqs2 database directory:\t%s'%mmseqsDbDir)
        print('Inxes MMseqs2 databases:\t{:s}'.format(str(create_idx)))
        print('MMseqs2 sensitivity (-s):\t%s'%str(sensitivity))
        print('Cutoff:\t%d'%cutoff)
        print('Confidence cutoff for paralogs:\t%s'%str(confCutoff))
        print('Length difference filtering threshold:\t%s'%str(lenDiffThr))
        print('Overwrite existing ortholog tables:\t%s'%overwrite_tbls)
        print('Overwrite everything:\t%s'%overwrite_all)
        print('Update an existing run:\t%s'%update_run)
        print('Keep raw MMseqs2 alignments:\t{:s}'.format(str(keepAlign)))

    #check the existence of the input directory
    if inDir[-1] != '/':
        inDir += '/'
    if not os.path.isdir(inDir):
        sys.stderr.write('ERROR: the directory with the input file %s was not found, please provide the path to a directory.\n'%inDir)
        sys.exit(-2)
    #check that the input directory is not empty
    tmpFlist = os.listdir(inDir)
    if ostype == 'Darwin':
        if '.DS_Store' in tmpFlist:
            tmpFlist.remove('.DS_Store')
    if len(tmpFlist) < 2:
        sys.stderr.write('ERROR: the directory with the input files only contains %d (%s) files\nPlease provide at least 2 proteomes.\n'%(len(tmpFlist), ', '.join(tmpFlist)))
        sys.exit(-2)
    #make sure that all the file do not contain file extensions nor '-' in their names
    for f in tmpFlist:
        if '-' in f:
            sys.stderr.write('ERROR: the file names including the \'-\' symbol (%s) are not allowed.\n Please remove the \'-\' from the species names.\n'%(f))
            sys.exit(-5)
        if '.' in f:
            sys.stderr.write('ERROR: the file (%s) includes the \'.\' symbol. This might reduce the readability of the output file names.\nPlease remove \'.\' from your input species names.\n'%(f))
            sys.exit(-5)
    #create the output directory if does not exist yet
    systools.makedir(outDir)
    if outDir[-1] != '/':
        outDir += '/'
    #check the existance if the shared directory
    if sharedDir is not None:
        if not os.path.isdir(sharedDir):
            sys.stderr.write('\nWARNING: the path to shared directory %s is not valid, a new directory will be created\n'%sharedDir)
            systools.makedir(sharedDir)
    else: #create it inside the output directory
        sharedDir = '%salignments/'%outDir
        systools.makedir(sharedDir)
        if debug:
            print('Shared directory created in %s'%sharedDir)

    # create the files with the sequence lengths
    spSizeDict = {} # contains the number fo sequences for each species
    spSizeDict = workers.write_seq_len_files_parallel(inDir, sharedDir, threads=threads, debug=debug)

    #check cutoff and woed size
    if cutoff < 30:
        cutoff = 40
    #sort the list with the input files (to avoid direfferent behaviours in OSX and Linux)
    tmpFlist.sort()
    #create the file with species names
    spFile = '%sspecies.txt'%(outDir)
    spList = []
    ofd = open(spFile, 'w')
    for f in tmpFlist:
        ofd.write('%s\n'%f)
        spList.append(f)
    ofd.close()
    del tmpFlist
    #generate file with the combinations
    spPairsFile = '%sspecies_pairs.txt'%(outDir)
    spPairs = list(itertools.combinations(spList, r=2))
    spPairs.sort()
    #check that the file with genome pairs has not been created yet
    if os.path.isfile(spPairsFile) and not overwrite_all and not update_run:
        pass
        #sys.stderr.write('WARNING: a species pair file (%s) with the species combination already exists and will not be recreated.\n'%spPairsFile)
    else:
        ofd = open(spPairsFile, 'w')
        [ofd.write('%s-%s\n'%(tpl[0], tpl[1])) for tpl in spPairs]
        ofd.close()
    #give some information about the combinations
    dashedPairs = ['%s-%s'%(tpl[0], tpl[1]) for tpl in spPairs]
    print('For the %d input species %d combinations are possible.'%(len(spList), len(spPairs)))
    # pair for which the ortholog table is missing
    requiredPairsDict = {}
    requiredSpDict = {} # species for which computations are required
    # create dictionary with both within- and between-proteome alignment names
    alignDict = {}
    requiredAlignDict = {} # will contain the required alignments
    sharedFlist = os.listdir(sharedDir) # contains files in the shared directory

    # set all the overwite flags to true if overwrite_all
    if overwrite_all:
        overwrite_tbls = True
    # generate all possible pairs
    for sp in spList:
        alignDict['{:s}-{:s}'.format(sp, sp)] = spSizeDict[sp]
    for pair in dashedPairs:
        sp1, sp2 = pair.split('-', 1)
        # calculate the genome size for the pair
        avgGenSize = float(spSizeDict[sp2] + spSizeDict[sp1]) / 2.
        alignDict[pair] = avgGenSize
        alignDict['{:s}-{:s}'.format(sp2, sp1)] = avgGenSize
    # fill the dictionaries with required alignments and ortholog tables
    if overwrite_all:
        #sys.exit('DEBUG :: overwrite_all')
        # it will be the same
        requiredAlignDict = copy.deepcopy(alignDict)
        for sp in spList:
            requiredSpDict[sp] = None
        requiredPairsDict = copy.deepcopy(dashedPairs)
        # remove all files in the shared directory
        sharedFlist = os.listdir(sharedDir)
        for f in sharedFlist:
            # keep the files with sequence lengths
            if f[-4:] == '.len':
                continue
            tmpPath = os.path.join(sharedDir, f)
            os.remove(tmpPath)
    # predict all ortholog tables but reuse alignments
    elif overwrite_tbls and not overwrite_all:
        # ortholog tables will be predicted for all pairs:
        for sp in spList:
            requiredSpDict[sp] = None
        requiredPairsDict = copy.deepcopy(dashedPairs)
        # alignments can be reused where possible
        for pair in alignDict:
            sp1, sp2 = pair.split('-', 1)
            # get the genome sizes
            sizeSp1 = spSizeDict[sp1]
            sizeSp2 = genSize = 0
            if sp1 == sp2:
                genSize = 2 * sizeSp1
            else:
                sizeSp2 = spSizeDict[sp2]
                genSize = sizeSp1 + sizeSp2
            # check alignment
            alignPath = os.path.join(sharedDir, '{:s}-{:s}'.format(sp1, sp2))
            if not os.path.isfile(alignPath) and not os.path.isfile('{:s}.gz'.format(alignPath)):
                requiredAlignDict['{:s}-{:s}'.format(sp1, sp2)] = genSize
        #sys.exit('DEBUG :: overwrite_tbls')
    # no overwrite, hence both ortholog tables
    # and alignments can be reused
    else:
        for pair in dashedPairs:
            tmpDir = os.path.join(outDir, pair)
            tblName = 'table.{:s}'.format(pair)
            sqlName = 'sqltable.{:s}'.format(pair)
            tmpTblPath = os.path.join(tmpDir, tblName)
            tmpSqlPath = os.path.join(tmpDir, sqlName)
            # if one of the tables does not exist
            if (not os.path.isfile(tmpTblPath)) or (not os.path.isfile(tmpSqlPath)):
                requiredPairsDict[pair] = None
                sp1, sp2 = pair.split('-', 1)
                # get the genome sizes
                sizeSp1 = spSizeDict[sp1]
                sizeSp2 = spSizeDict[sp2]
                # calculate the average genome sizes per pair
                avgSizeSp11 = sizeSp1
                avgSizeSp22 = sizeSp2
                avgSizeBtw = float(sizeSp1 + sizeSp2) / 2.
                # check alignment AB
                alignPathAB = os.path.join(sharedDir, '{:s}-{:s}'.format(sp1, sp2))
                if not os.path.isfile(alignPathAB) and not os.path.isfile('{:s}.gz'.format(alignPathAB)):
                    requiredAlignDict['{:s}-{:s}'.format(sp1, sp2)] = avgSizeBtw
                # check alignment BA
                alignPathBA = os.path.join(sharedDir, '{:s}-{:s}'.format(sp2, sp1))
                if not os.path.isfile(alignPathBA) and not os.path.isfile('{:s}.gz'.format(alignPathBA)):
                    requiredAlignDict['{:s}-{:s}'.format(sp2, sp1)] = avgSizeBtw
                # check alignment AA
                alignPathAA = os.path.join(sharedDir, '{:s}-{:s}'.format(sp1, sp1))
                if not os.path.isfile(alignPathAA) and not os.path.isfile('{:s}.gz'.format(alignPathAA)):
                    requiredAlignDict['{:s}-{:s}'.format(sp1, sp1)] = sizeSp1
                # check alignment BB
                alignPathBB = os.path.join(sharedDir, '{:s}-{:s}'.format(sp2, sp2))
                if not os.path.isfile(alignPathBB) and not os.path.isfile('{:s}.gz'.format(alignPathBB)):
                    requiredAlignDict['{:s}-{:s}'.format(sp2, sp2)] = sizeSp2
        # fill the dict with required species
        for pair in requiredAlignDict:
            sp1, sp2 = pair.split('-', 1)
            if not sp1 in requiredSpDict:
                requiredSpDict[sp1] = spSizeDict[sp1]
            if not sp2 in requiredSpDict:
                requiredSpDict[sp2] = spSizeDict[sp2]
            # if all the possible species are already include exit loop
            if len(requiredSpDict) == len(spList):
                break

    #sys.exit('DEBUG :: run_sonicparanoid2_multiproc')
    # check which if alignments are already available
    totAlign = len(alignDict)
    print('\nTotal possible alignments:\t{:d}'.format(totAlign))
    print('Required alignments:\t{:d}'.format(len(requiredAlignDict)))
    print('Required ortholog table predictions:\t{:d}'.format(len(requiredPairsDict)))

    if len(requiredAlignDict) > 0:
        #NOTE: Restore if the new version is not better
        #'''
        # sort the alignments so that the biggest ones are performed first
        s = [(k, requiredAlignDict[k]) for k in sorted(requiredAlignDict, key=requiredAlignDict.get, reverse=True)]
        # empty the dictionary and fill it again with the size-sorted one
        requiredAlignDict.clear()
        requiredAlignDict = {key: value for (key, value) in s}
        del s
        #'''
        dqAlign = deque()

        for p, size in requiredAlignDict.items():
            tpl = (p, size)
            dqAlign.append(tpl)

        #'''
        pairsCnt = len(requiredAlignDict)
        jobCnt = 0
        n = 1
        chunkList = [] # will contain the size of chunks that will fill the job queue
        # now create a list with the chunk sizes
        while jobCnt < pairsCnt:
            n += 1
            triangularNum = int((n * (n + 1)) / 2.)
            jobCnt += triangularNum
            chunkList.append(triangularNum)
        # sort the list of chunks in inverted order
        chunkList.sort(reverse=True)
        # remove the biggest chunk
        chunkList.pop(0)
        # make a copy with the chunks and invert it
        chunkListInv = []
        for el in chunkList:
            chunkListInv.append(el)
        chunkListInv.sort()
        # set the step to half of the cpus
        heavyChunkSize = int(threads / 2.)
        if heavyChunkSize == 0:
            heavyChunkSize = 1
        # update the alignments dictionary
        requiredAlignDict.clear()
        remainingJobs = len(dqAlign)
        while remainingJobs > 0:
            # add the chunk of jobs that require a lot of memory
            for i in range(0, heavyChunkSize):
                if len(dqAlign) > 0:
                    p, fSize = dqAlign.popleft()
                    requiredAlignDict[p] = fSize
                    remainingJobs -= 1 # decrement
                else: # no more elements to be added
                    break
            # add a chunk of small jobs
            if len(chunkList) > 0:
                if len(dqAlign) > 0:
                    cSize = chunkList.pop(0)
                    for i in range(0, cSize):
                        if len(dqAlign) > 0:
                            p, fSize = dqAlign.pop()
                            requiredAlignDict[p] = fSize
                            remainingJobs -= 1 # decrement
                        else: # no more elements to be added
                            break
            # add chunks of growing size
            elif len(chunkListInv) > 0:
                if len(dqAlign) > 0:
                    cSize = chunkListInv.pop(0)
                    for i in range(0, cSize):
                        if len(dqAlign) > 0:
                            p, fSize = dqAlign.pop()
                            requiredAlignDict[p] = fSize
                            remainingJobs -= 1 # decrement
                        else: # no more elements to be added
                            break

        # perform alignments
        #sys.exit('DEBUG :: run_sonicparanoid2_multiproc')
        workers.perform_mmseqs_multiproc_alignments(requiredAlignDict, inDir, sharedDir, mmseqsDbDir, create_idx=create_idx, sensitivity=sensitivity, cutoff=cutoff, threads=threads, keepAlign=keepAlign, debug=debug)
    # perform the orthology inference for the required pairs
    if len(requiredPairsDict) > 0:
        #print(requiredPairsDict)

        #'''
        # sort the the species dictionary by size
        s = [(k, spSizeDict[k]) for k in sorted(spSizeDict, key=spSizeDict.get, reverse=True)]
        # empty the dictionary and fill it again with the size-sorted one
        spSizeDict.clear()
        spSizeDict = {key: value for (key, value) in s}
        del s

        # add counters for the within alignments to be loaded
        withinAlignDict = copy.deepcopy(spSizeDict)
        del spSizeDict

        # Prepare the dictionary that will contain the withinalignment scores
        # set the counters to 0
        for sp in withinAlignDict:
            withinAlignDict[sp] = [0, None, None]
        # fill the dict with required species
        for pair in requiredPairsDict:
            sp1, sp2 = pair.split('-', 1)
            # increment the counters
            withinAlignDict[sp1][0] += 1
            withinAlignDict[sp2][0] += 1

        sys.stdout.write('\nPredicting {:d} ortholog tables...'.format(len(requiredPairsDict)))
        # calculate cpu-time for orthology inference
        orthology_start = time.perf_counter()

        #'''#### USE PREPROCESSING ####
        #### ORIGINAL ####
        # segOverlapCutoff: float = 0.5
        ##################
        segOverlapCutoff: float = 0.25
        # The actual matching segments must cover this of this match of the matched sequence
        # For example for a matched sequence 70 bps long, segments 1-15 and 50-70 gives a total coverage of 35, which is 50% of total.
        segCoverageCutoff: float = 0.25
        # load the required within alignments in parallel
        inpyranoid.preprocess_within_alignments_parallel(withinAlignDict, alignDir=sharedDir, threads=threads, covCoff=segCoverageCutoff, overlapCoff=segOverlapCutoff, debug=False)
        #sys.exit('DEBUG :: run_sonicparanoid2_multiproc :: after preprocessing withing alignments')
        workers.perform_parallel_orthology_inference_shared_dict(requiredPairsDict, inDir, outDir=outDir, sharedDir=sharedDir, sharedWithinDict=withinAlignDict, cutoff=cutoff, confCutoff=confCutoff, lenDiffThr=lenDiffThr, threads=threads, debug=debug)
        #'''##################################

        '''#### DO NOT USE PREPROCESSING ####
        workers.perform_parallel_orthology_inference(requiredPairsDict, inDir, outDir=outDir, sharedDir=sharedDir, cutoff=cutoff, confCutoff=confCutoff, lenDiffThr=lenDiffThr, threads=threads, debug=debug)
        #'''##################################

        sys.stdout.write('\nOrtholog tables creation elapsed time (seconds):\t{:s}\n'.format(str(round(time.perf_counter() - orthology_start, 3))))
        #sys.exit('DEBUG :: ortholog_detection :: run_sonicparanoid2_multiproc :: after orthology inference')
    # return the paths for species and pairs files
    return (spFile, spPairsFile)



def run_sonicparanoid2_parallel_mmseqs(inSeq1, inSeq2, outDir=os.getcwd(), threads=1, sharedDir=None, mmseqsDbDir=None, sensitivity=4.0, cutoff=40, confCutoff=0.05, lenDiffThr=0.5, reuse=True, noMmseqs=False, debug=False):
    """Execute sonicparanoid using MMseqs2 for the sequence alignment."""
    if debug:
        print('run_sonicparanoid2_parallel_mmseqs :: START')
        print('Input seq1:%s'%inSeq1)
        print('Input seq2:%s'%inSeq2)
        print('Outdir:%s'%outDir)
        print('CPUs (for mmseqs):\t%d'%threads)
        print('Shared output directory:\t%s'%sharedDir)
        print('Directory containing the databases for MMseqs22:\t%s'%mmseqsDbDir)
        print('MMseqs2 sensitivity (-s):\t%s'%str(sensitivity))
        print('Cutoff:\t%d'%cutoff)
        print('Confidence cutoff for paralogs:\t%s'%str(confCutoff))
        print('Length difference filtering threshold:\t%s'%str(lenDiffThr))
        print('Reuse mode:\t%s'%reuse)
        print('No MMseqs2:\t%s'%noMmseqs)
    # check the existence of the input file
    if not os.path.isfile(inSeq1):
        sys.stderr.write('ERROR: The first input file %s was not found, please provide the path to a valid file.\n'%inSeq1)
        sys.exit(-2)
    if not os.path.isfile(inSeq2):
        sys.stderr.write('ERROR: The second input file %s was not found, please provide the path to a valid file.\n'%inSeq2)
        sys.exit(-2)
    #create the output directory if does not exist yet
    if outDir != os.getcwd():
        if not os.path.isdir(outDir):
            systools.makedir(outDir)
    if outDir[-1] != '/':
        outDir += '/'
    # check the existance if the shared directory
    if sharedDir is not None:
        if not os.path.isdir(sharedDir):
            sys.stderr.write('\nWARNING: the path to shared directory %s is not valid, a new directory will be created\n'%sharedDir)
            systools.makedir(sharedDir)
    else: #create it inside the output directory
        sharedDir = '%sshared_output/'%outDir
        systools.makedir(sharedDir)
        if debug:
            print('Shared directory created in %s'%sharedDir)
    # check the existance if the mmseqs database directory
    if mmseqsDbDir is not None:
        if not os.path.isdir(mmseqsDbDir):
            sys.stderr.write('\nWARNING: the path to directory containing the MMseqs2 databases\n%s\nis not valid, a new directory will be created\n'%mmseqsDbDir)
            systools.makedir(mmseqsDbDir)
    else: #create it inside the output directory
        mmseqsDbDir = '%smmseqs2_databases/'%outDir
        systools.makedir(mmseqsDbDir)
        if debug:
            print('Directory for MMseqs2 databases created in %s'%mmseqsDbDir)
    # check if the output directory differs from the input one
    if os.path.dirname(inSeq1) + '/' == outDir:
        sys.stderr.write('\nERROR: the output directory %s\nmust be different from the one in which the input files are stored.\n')
        sys.exit(-2)
    if os.path.dirname(inSeq2) + '/' == outDir:
        sys.stderr.write('\nERROR: the output directory %s\nmust be different from the one in which the input files are stored.\n')
        sys.exit(-2)
    # check cutoff
    if cutoff < 30:
        cutoff = 40
    # extract species names
    outName1 = os.path.basename(inSeq1)
    outName2 = os.path.basename(inSeq2)
    #copy the input files to the output directory
    tmpIn1 = os.path.join(outDir, outName1)
    tmpIn2 = os.path.join(outDir, outName2)
    systools.copy(inSeq1, tmpIn1, metaData=True, debug=debug)
    systools.copy(inSeq2, tmpIn2, metaData=True, debug=debug)
    outAA = '%s-%s'%(outName1, outName1)
    tmpDirAA = 'tmp_{:s}'.format(outAA)
    outAB = '%s-%s'%(outName1, outName2)
    tmpDirAB = 'tmp_{:s}'.format(outAB)
    outBA = '%s-%s'%(outName2, outName1)
    tmpDirBA = 'tmp_{:s}'.format(outBA)
    outBB = '%s-%s'%(outName2, outName2)
    tmpDirBB = 'tmp_{:s}'.format(outBB)
    outTable = 'table.%s-%s'%(outName1, outName2)
    outSql = 'sqltable.%s-%s'%(outName1, outName2)
    executionTime = 'execution_time.%s-%s'%(outName1, outName2)
    # find the mmseqs2 parser in the source directory
    pySrcDir = os.path.dirname(os.path.abspath(__file__))
    mmseqsparser = os.path.join(pySrcDir, 'mmseqs_parser_cython.py')
    # copy the file to the output directory
    systools.copy(mmseqsparser, outDir, metaData=False, debug=False)
    mmseqsparser = '%smmseqs_parser_cython.py'%outDir
    os.chmod(mmseqsparser, 0o751)
    # find and copy the parser module
    tmpListDir = os.listdir(pySrcDir)
    parserModuleFound = False
    for el in tmpListDir:
        if el.startswith('mmseqs_parser_c'):
            if el.endswith('.pyx') or el.endswith('.c'):
                continue
            systools.copy(os.path.join(pySrcDir, el), outDir, metaData=False, debug=False)
            os.path.join(pySrcDir, el)
            parserModuleFound = True
    # make sure that the module has been found...
    if not parserModuleFound:
        sys.stderr.write('\nERROR: the mmseqs2 parser could not be found in the main source directory.\n')
        sys.stderr.write('Try to execute the SonicParanoid setup by running the script setup_sonicparanoid.py.\n')
        sys.exit(-5)
    #change the current working directory
    prevCwd = os.getcwd()
    os.chdir(outDir)
    #start the timing
    start_time = time.time()
    # NOTE: this could be imrpoved by passing the sequences as a dictionary
    # the sequence length could laternatively written on separate files in the input directory
    ##### Execute the required mmseqs2 runs ######
    in1Dict = load_input_sequences(tmpIn1, debug=debug)
    in2Dict = load_input_sequences(tmpIn2, debug=debug)
    #set the execution time variables
    outPathAA = outPathBB = outPathBA = outPathAB = None
    tot_mmseqs_time_AA = mmseqs1_time_AA = mmseqs2_time_AA = 0
    tot_mmseqs_time_BB = mmseqs1_time_BB = mmseqs2_time_BB = 0
    mmseqs1_time_AB = mmseqs2_time_AB = tot_mmseqs_time_AB = 0
    mmseqs1_time_BA = mmseqs2_time_BA = tot_mmseqs_time_BA = 0
    #Execute the MMseqs2 if required
    if not noMmseqs:
        #compare AA
        if reuse:
            oldAA = '%s%s'%(sharedDir, outAA)
            if os.path.isfile(oldAA):
                systools.copy(oldAA, outDir, metaData=True)
                if debug:
                    sys.stdout.write('\nSuccessfully copied alignment files for\n%s\nto the run directory\n%s\n'%(oldAA, outDir))
                    sys.stdout.write('The MMseqs2 alignment for %s can be skipped!\n'%(outAA))
                tot_mmseqs_time_AA = mmseqs1_time_AA = mmseqs2_time_AA = 0
                outPathAA = '%s%s'%(outDir, outAA)
            else: #the run is required
                outPathAA, mmseqs1_time_AA, mmseqs2_time_AA, tot_mmseqs_time_AA = mmseqs_1pass(tmpIn1, tmpIn1, dbDir=mmseqsDbDir, outDir=outDir, tmpDirName=tmpDirAA, sensitivity=sensitivity, evalue=1000, cutoff=cutoff, threads=threads, debug=debug)
                if not os.path.isfile('%s%s'%(sharedDir, os.path.basename(outPathAA))):
                    #copy the generated file to the shared directory
                    systools.copy(outPathAA, sharedDir,  metaData=True)
        else: #just compare the 2
            outPathAA, mmseqs1_time_AA, mmseqs2_time_AA, tot_mmseqs_time_AA = mmseqs_1pass(tmpIn1, tmpIn1, dbDir=mmseqsDbDir, outDir=outDir, tmpDirName=tmpDirAA, sensitivity=sensitivity, evalue=1000, cutoff=cutoff, threads=threads, debug=debug)
        #compare AB
        outPathAB, mmseqs1_time_AB, mmseqs2_time_AB, tot_mmseqs_time_AB = mmseqs_1pass(tmpIn1, tmpIn2, dbDir=mmseqsDbDir, outDir=outDir, tmpDirName=tmpDirAB, sensitivity=sensitivity, evalue=1000, cutoff=cutoff, threads=threads, debug=debug)
        #compare BA
        outPathBA, mmseqs1_time_BA, mmseqs2_time_BA, tot_mmseqs_time_BA = mmseqs_1pass(tmpIn2, tmpIn1, dbDir=mmseqsDbDir, outDir=outDir, tmpDirName=tmpDirBA, sensitivity=sensitivity, evalue=1000, cutoff=cutoff, threads=threads, debug=debug)
        #sys.exit('DEBUG :: run_sonicparanoid2_parallel_mmseqs [after the AA, AB and BA alignments]')
        #compare BB
        if reuse:
            oldBB = '%s%s'%(sharedDir, outBB)
            if os.path.isfile(oldBB):
                systools.copy(oldBB, outDir)
                if debug:
                    sys.stdout.write('\nSuccessfully copied alignment files output for\n%s\nto the run directory\n%s\n'%(oldBB, outDir))
                    sys.stdout.write('The MMseqs2 run for %s can be skipped!\n'%(outBB))
                tot_mmseqs_time_BB = mmseqs1_time_BB = mmseqs2_time_BB = 0
                outPathBB = '%s%s'%(outDir, outBB)
            else: #the run is required
                outPathBB, mmseqs1_time_BB, mmseqs2_time_BB, tot_mmseqs_time_BB = mmseqs_1pass(tmpIn2, tmpIn2, dbDir=mmseqsDbDir, outDir=outDir, tmpDirName=tmpDirBB, sensitivity=sensitivity, evalue=1000, cutoff=cutoff, threads=threads, debug=debug)
                if not os.path.isfile('%s%s'%(sharedDir, os.path.basename(outPathBB))):
                    #copy the generated to the shared directory
                    systools.copy(outPathBB, sharedDir,  metaData=True)
        else: #just compare the 2
            outPathBB, mmseqs1_time_BB, mmseqs2_time_BB, tot_mmseqs_time_BB = mmseqs_1pass(tmpIn2, tmpIn2, dbDir=mmseqsDbDir, outDir=outDir, tmpDirName=tmpDirBB, sensitivity=sensitivity, evalue=1000, cutoff=cutoff, threads=threads, debug=debug)
    #sys.exit('DEBUG :: run_sonicparanoid2_parallel_mmseqs [after the alignments]')
    #check that the output tables for AA, AB, BA and BB do exist
    if outPathAA is None:
        outPathAA = '%s%s'%(outDir, outAA)
    if not os.path.isfile(outPathAA):
        sys.stderr.write('ERROR: the alignment file for AA (%s-%s) is missing, please create it using MMseqs2.\n'%(outName1, outName1))
        sys.exit(-2)
    if outPathBB is None:
        outPathBB = '%s%s'%(outDir, outBB)
    if not os.path.isfile(outPathBB):
        sys.stderr.write('ERROR: the alignment file for BB (%s-%s) is missing, please create it using MMseqs2.\n'%(outName2, outName2))
        sys.exit(-2)
    if outPathAB is None:
        outPathAB = '%s%s'%(outDir, outAB)
    if not os.path.isfile(outPathAB):
        sys.stderr.write('ERROR: the alignment file for AB (%s-%s) is missing, please create it using MMseqs2.\n'%(outName1, outName2))
        sys.exit(-2)
    if outPathBA is None:
        outPathBA = '%s%s'%(outDir, outBA)
    if not os.path.isfile(outPathBA):
        sys.stderr.write('ERROR: the alignment file for BA (%s-%s) is missing, please create it using MMseqs2.\n'%(outName2, outName1))
        sys.exit(-2)
    # infer orthologs
    orthology_prediction_start = time.time()
    inpyranoid.infer_orthologs(tmpIn1, tmpIn2, outDir=outDir, confCutoff=confCutoff, lenDiffThr=lenDiffThr, debug=debug)
    #check that all the files have been created
    if not os.path.isfile(outTable):
        sys.stderr.write('WARNING: the inparanoid output table file %s was not generated.'%outTable)
        outTable = None
    if not os.path.isfile(outSql):
        sys.stderr.write('WARNING: the inparanoid sql table %s was not generated.'%outSql)
        outSql = None
    #everything went ok!
    end_time = time.time()
    orthology_prediction_tot = round(end_time - orthology_prediction_start, 2)
    if debug:
        sys.stdout.write('\nOrthology prediction elapsed time (seconds):\t%s\n'%str(orthology_prediction_tot))
    tot_time = round(end_time - start_time, 2)
    if debug:
        sys.stdout.write('\nTotal elapsed time (seconds):\t%s\n'%str(tot_time))
    #sys.exit('\nDEBUG :: run_sonicparanoid2_parallel_mmseqs [after execution of orhtology inference]')
    #write the file with execution time
    executionTimePath = '%s%s'%(outDir, executionTime)
    ofd = open(executionTimePath, 'w')
    ofd.write('input1\tinput2\tinput1_size\tinput1_size\talign_AA_pass1\talign_AA_pass2\talign_AA_tot\talign_AB_pass1\talign_AB_pass2\talign_AB_tot\talign_BA_pass1\talign_BA_pass2\talign_BA_tot\talign_BB_pass1\talign_BB_pass2\talign_BB_tot\tortholog_search\ttotal_runtime\n')
    #now write the values
    ofd.write('%s\t%s\t%d\t%d\t%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s\n'%(os.path.basename(tmpIn1), os.path.basename(tmpIn2), len(in1Dict), len(in2Dict), mmseqs1_time_AA, mmseqs2_time_AA, tot_mmseqs_time_AA, mmseqs1_time_AB, mmseqs2_time_AB, tot_mmseqs_time_AB, mmseqs1_time_BA, mmseqs2_time_BA, tot_mmseqs_time_BA, mmseqs1_time_BB, mmseqs2_time_BB, tot_mmseqs_time_BB, orthology_prediction_tot, tot_time))
    ofd.close()
    os.chdir(prevCwd)
    # cleanup! remove input files and mmseqs database files
    # copied input
    if os.path.isfile(tmpIn1):
        os.remove(tmpIn1)
    if os.path.isfile(tmpIn2):
        os.remove(tmpIn2)
    #sys.exit('\nDEBUG :: run_sonicparanoid2_parallel_mmseqs [A vs B, final part]')
    #return the output paths
    return(outTable, outSql, None, tot_time, True)



def set_mmseqs_path(newPath):
    global mmseqsPath
    """Set path of the directory in which MMseqs2 binaries are stored."""
    mmseqsPath = os.path.normpath(newPath)



def test_blast_2pass(local=False, debug=True):
    """Test execution of 2-pass blast similar to the implementation in core-inparanoid."""
    #blast_2pass(querySeq, dbSeq, querySeqDict, dbSeqDict, scoreCoff=40, outName=None, outDir=os.getcwd(), threads=4, debug=False)
    root = inputDir = outDir = in1 = in2 = None
    if not local:
        root = '/home/salvocos/tmp/test_ortholog_detection/'
        inputDir = '%sinput/'%root
        outDir = '%soutput/'%root
    else: #run in local mac computer
        root = '/Users/salvocos/src_repo/pyinparanoid/'
        inputDir = '%sinput/'%root
        outDir = '%stest_blast2pass/'%root
    #in1 = '%sJCM30591_1000'%(inputDir)
    #in2 = '%sJCM30689_1000'%(inputDir)
    #in1 = '%sJCM30591'%(inputDir)
    #in2 = '%sJCM30689'%(inputDir)
    in1 = '%sjcm_10049'%(inputDir)
    in2 = '%sjcm_12878'%(inputDir)
    #output proteins
    #load the input in dictionaries
    in1Dict = load_input_sequences(in1, debug=debug)
    in2Dict = load_input_sequences(in2, debug=debug)
    cpus = 6
    outPath, tot_time, blast1_time, blast2_time = blast_2pass(in1, in2, in1Dict, in2Dict, scoreCoff=40, outName='%s-%s'%(os.path.basename(in1), os.path.basename(in2)), outDir=outDir, threads=cpus, debug=debug)
    print(outPath)



def test_calc_inparanoid_exec_time(debug=True):
    """Test execution time calculation."""
    root = '/home/salvocos/tmp/test_ortholog_detection/'
    #runInparanoid(inSeq1, inSeq2, inParanoidPackage, outDir=os.getcwd(), threads=4, debug=True):
    inputDir = '%stest_inparalog/'%root
    #outDir = '%soutput/'%root
    outDir = inputDir
    calc_inparanoid_exec_time(inputDir, outDir=inputDir, debug=debug)



def test_calc_ortholog_group_stats(debug=True):
    """Calculate simple stats about ortholog pairs."""
    rootDir = '/home/salvocos/projects/pyinparanoid/benchmark/plastparanoid_runs/sonicparanoid_bacteria/'
    pairsFile = '%sspecies_pairs.txt'%rootDir
    outDir = '%sortholog_pairs/'%rootDir
    #extract pairs
    calc_ortholog_group_stats(rootDir=rootDir, outDir=outDir, outName=None, pairsFile=pairsFile, debug=debug)



def test_copy_quickparanoid_files(debug=False):
    """Test copy of quick multiparanoid files"""
    srcDir = '/Users/salvocos/src_repo/sonicparanoid/quick_multi_paranoid/'
    outDir = '/Users/salvocos/Desktop/quickParanoid_test_output/'
    copy_quickparanoid_files(srcDir, outDir, debug=debug)



def test_extract_ortholog_pairs(debug=True):
    """Extract ortholog pairs."""
    rootDir = '/home/salvocos/projects/pyinparanoid/benchmark/runs/qfo2011_fungi_pyparanoid/'
    pairsFile = '%sspecies_pairs.txt'%rootDir
    outDir = '/home/salvocos/projects/pyinparanoid/benchmark/runs/qfo2011_fungi_pyparanoid/ortholog_pairs/'
    coreOnly = False
    splitHdr = True
    #extract pairs
    extract_ortholog_pairs(rootDir=rootDir, outDir=outDir, outName=None, pairsFile=pairsFile, coreOnly=coreOnly, splitMode=splitHdr, debug=debug)



def test_fetch_inparanoid_tables(debug=True):
    """Find ortholog relation tables."""
    rootDir = '/home/salvocos/projects/pyinparanoid/benchmark/runs/qfo2011_fungi_pyparanoid/'
    pairsFile = '%sspecies_pairs.txt'%rootDir
    outDir = '/home/salvocos/projects/pyinparanoid/benchmark/runs/fungi_2011/to_be_removed/'
    prefix = 'Output'
    #prefix = 'table'
    tblList = fetch_inparanoid_tables(rootDir=rootDir, outDir=outDir, pairsFile=pairsFile, tblPrefix=prefix, debug=debug)



def test_fetch_sql_files(debug=True):
    """Find SQL tables."""
    core = False
    rootDir = '/home/salvocos/projects/pyinparanoid/benchmark/runs/fungi_2011/'
    pairsFile = '%sspecies_pairs.txt'%rootDir
    outDir = '/home/salvocos/projects/pyinparanoid/benchmark/runs/fungi_2011/test_quickparanoid/'
    if core:
        outDir = '/home/salvocos/projects/pyinparanoid/benchmark/runs/fungi_2011/test_quickparanoid_core/'
    fetch_sql_files(rootDir=rootDir, outDir=outDir, pairsFile=pairsFile, coreOnly=core, debug=debug)



def test_filter_sql_tbl_core_orthologs(debug=True):
    """Filter table and keep only CORE orthologs."""
    inTbl = '/home/salvocos/projects/pyinparanoid/benchmark/runs/fungi_2011/test_quickparanoid/sqltable.schizosaccharomyces_pombe-phaeosphaeria_nodorum'
    filter_sql_tbl_core_orthologs(inTbl=inTbl, debug=debug)



def test_inparanoid_like_parser(debug=True):
    """Test extraction of alignment information from Blastp tab-separated output."""
    root = '/home/salvocos/tmp/test_ortholog_detection/'
    inputDir = '%sinput/'%root
    outDir = '%soutput/'%root
    cutoff = 40
    plastMode = True

    if not plastMode:
        #AA
        blastOutPathAA = '%saquifex_aeolicus-aquifex_aeolicus_blastp_outfmt6_required.tsv'%inputDir
        outputName = 'aquifex_aeolicus-aquifex_aeolicus'
        inparanoid_like_parser(blastOutPathAA, outDir=outDir, outName=outputName, scoreCoff=cutoff, debug=debug)
        print('\nAA done! ')
        #BB
        blastOutPathBB = '%sthermotoga_maritima-thermotoga_maritima_blastp_outfmt6_required.tsv'%inputDir
        outputName = 'thermotoga_maritima-thermotoga_maritima'
        inparanoid_like_parser(blastOutPathBB, outDir=outDir, outName=outputName, scoreCoff=cutoff, debug=debug)
        #AB
        blastOutPathAB = '%saquifex_aeolicus-thermotoga_maritima_blastp_outfmt6_required.tsv'%inputDir
        outputName = 'aquifex_aeolicus-thermotoga_maritima'
        inparanoid_like_parser(blastOutPathAB, outDir=outDir, outName=outputName, scoreCoff=cutoff, debug=debug)
        #BA
        blastOutPathBA = '%sthermotoga_maritima-aquifex_aeolicus_blastp_outfmt6_required.tsv'%inputDir
        outputName = 'thermotoga_maritima-aquifex_aeolicus'
        inparanoid_like_parser(blastOutPathBA, outDir=outDir, outName=outputName, scoreCoff=cutoff, debug=debug)
    else:
        #AA
        blastOutPathAA = '%saquifex_aeolicus-aquifex_aeolicus_plastp_outfmt2_Fyes.tsv'%inputDir
        outputName = 'aquifex_aeolicus-aquifex_aeolicus'
        inparanoid_like_parser(blastOutPathAA, outDir=outDir, outName=outputName, scoreCoff=cutoff, plastMode=True, debug=debug)
        #BB
        blastOutPathBB = '%sthermotoga_maritima-thermotoga_maritima_plastp_outfmt2_Fyes.tsv'%inputDir
        outputName = 'thermotoga_maritima-thermotoga_maritima'
        inparanoid_like_parser(blastOutPathBB, outDir=outDir, outName=outputName, scoreCoff=cutoff, plastMode=True, debug=debug)
        #AB
        blastOutPathAB = '%saquifex_aeolicus-thermotoga_maritima_plastp_outfmt2_Fyes.tsv'%inputDir
        outputName = 'aquifex_aeolicus-thermotoga_maritima'
        inparanoid_like_parser(blastOutPathAB, outDir=outDir, outName=outputName, scoreCoff=cutoff, plastMode=True, debug=debug)
        #BA
        blastOutPathBA = '%sthermotoga_maritima-aquifex_aeolicus_plastp_outfmt2_Fyes.tsv'%inputDir
        outputName = 'thermotoga_maritima-aquifex_aeolicus'
        inparanoid_like_parser(blastOutPathBA, outDir=outDir, outName=outputName, scoreCoff=cutoff, plastMode=True, debug=debug)



def test_mmseqs_1pass(debug=True):
    """Test alignment with mmseqs and parsing."""
    # mmseqs_1pass(inSeq, dbSeq, dbDir=os.getcwd(), outDir=os.getcwd(), tmpDirName=None, sensitivity=4.0, evalue=1000, cutoff=40, threads=4, debug=False):
    root = inDir = outDir = in1 = in2 = None
    root = '/home/salvocos/tmp/test_ortholog_detection/'
    inDir = os.path.join(root, 'input/')
    outDir = os.path.join(root, 'output/')
    dbDir = os.path.join(root, 'mmseqs_dbs/')
    #input files
    sp1 = 'gloeobacter_violaceus'
    sp2 = 'thermotoga_maritima'
    in1 = os.path.join(inDir, sp1)
    in2 = os.path.join(inDir, sp2)
    AA = '{:s}-{:s}'.format(sp1, sp1)
    BB = '{:s}-{:s}'.format(sp2, sp2)
    AB = '{:s}-{:s}'.format(sp1, sp2)
    BA = '{:s}-{:s}'.format(sp2, sp1)
    # settings
    cpus = 8
    cutoff = 40
    # AA
    tmpDir = 'tmp_{:s}'.format(AA)
    mmseqs_1pass(in1, in2, dbDir=dbDir, outDir=outDir, tmpDirName=tmpDir, sensitivity=4.0, evalue=1000, cutoff=cutoff, threads=cpus, debug=debug)



def test_run_quickparanoid(debug=True):
    """Test multi-species ortholog inference."""
    srcDir = '/Users/salvocos/src_repo/sonicparanoid/quick_multi_paranoid/'
    tblDir = '/Users/salvocos/Desktop/quickParanoid_test_output/'
    species = '%sspecies.txt'%tblDir
    run_quickparanoid(sqlTblDir=tblDir, outDir=tblDir, srcDir=srcDir, outName=None, speciesFile=species, debug=debug)



def test_run_inparanoid(debug=True):
    """Test the function to predict genes using Augustus."""
    #fasta
    root = '/home/salvocos/tmp/test_ortholog_detection/'
    #runInparanoid(inSeq1, inSeq2, inParanoidPackage, outDir=os.getcwd(), threads=4, debug=True):
    inputDir = '%sinput/'%root
    outDir = '%soutput/'%root
    in1 = '%sJCM30591_1000'%(inputDir)
    in2 = '%sJCM30689_1000'%(inputDir)
    #output proteins
    #core inparanoid using blastall
    inpaPckg = '/home/salvocos/tools/packages/inparanoid_core_timing.tar.gz'
    tbl, sql, summary, tot_time, success = run_inparanoid(in1, in2, inParanoidPackage=get_inparanoid_pckg(), outDir=outDir, threads=4, debug=debug)



def test_run_inparanoid_parallel(local=False, debug=True):
    """Test execution of inparanoid using parallel blast runs."""
    root = '/home/salvocos/tmp/test_ortholog_detection/'
    inputDir = '%sinput/'%root
    outDir = '%soutput/'%root



def test_run_pyparanoid(local=False, debug=True):
    """Test execution of pyinparanoid on a set of input files."""
    #run_pyparanoid(inDir, inParanoidPackage, outDir=os.getcwd(), threads=4, sharedDir=None, reuse=True, noBlast=False, memMode=False, bootstrap=False, debug=False)
    root = inputDir = outDir = sharedDir = in1 = in2 = None
    if not local:
        root = '/home/salvocos/tmp/test_ortholog_detection/'
        inputDir = '%sinput/'%root
        sharedDir = '%sshared_output/'%root
        outDir = '%soutput/'%root
    else: #run in local mac computer
        root = '/Users/salvocos/src_repo/pyinparanoid/'
        inputDir = '%spyparanoid_input_small/'%root
        sharedDir = '%spyparanoid_shared_output/'%root
        outDir = '%spyparanoid_output/'%root
    #set main variables
    cpus = 2
    btstrp = True
    inParanoidPackage = None
    reuse = True
    skipBlast = True
    btstrp = True
    rerun = True
    if not local:
        inParanoidPackage = '/home/salvocos/tools/packages/inparanoid_mod_cmd_params.tar.gz'
    else:
        inParanoidPackage = '/Users/salvocos/src_repo/pyinparanoid/inparanoid_mod_cmd_params.tar.gz'
    run_pyparanoid(inputDir, inParanoidPackage=inParanoidPackage, outDir=outDir, threads=cpus, sharedDir=sharedDir, reuse=True, noBlast=skipBlast, memMode=False, bootstrap=btstrp, overwrite=rerun, debug=debug)



def test_run_sonicparanoid2_parallel_mmseqs(debug=False):
    """Test sonicapranoid using mmseqs2 on a input dataset."""
    root = '/home/salvocos/tmp/test_ortholog_detection/'
    inDir = os.path.join(root, 'input/')
    outRoot = os.path.join(root, 'test_run/')
    cpus = 8
    sens = 4
    # input
    #input files
    A = 'gloeobacter_violaceus'
    B = 'thermotoga_maritima'
    inA = os.path.join(inDir, A)
    inB = os.path.join(inDir, B)

    # fungi_2011
    #A = '%saspergillus_fumigatus'%inDir
    #B = '%scandida_albicans'%inDir

    dbDir = os.path.join(outRoot, 'mmseqs2_databases/')
    sharedDir = '{:s}shared_output/'.format(outRoot)
    outDir = '{:s}{:s}-{:s}'.format(outRoot, A, B)
    run_sonicparanoid2_parallel_mmseqs(inA, inB, outDir=outDir, threads=cpus, sharedDir=sharedDir, mmseqsDbDir=dbDir, sensitivity=sens, cutoff=40, confCutoff=0.05, lenDiffThr=0.5, reuse=True, noMmseqs=False, debug=debug)

    #run_sonicparanoid2_parallel_mmseqs(inA, inB, outDir=outDir, threads=cpus, sharedDir=None, mmseqsDbDir=None, sensitivity=4.0, cutoff=40, confCutoff=0.05, lenDiffThr=0.5, reuse=True, noMmseqs=False, debug=False):



def test_run_sonicparanoid2(debug=False):
    """Test the alignment creation of inpranoid compatible files using mmseqs2."""
    root = '/home/salvocos/tmp/test_ortholog_detection/'
    inDir = os.path.join(root, 'input/')
    #inDir = '/home/salvocos/projects/pyinparanoid/benchmark/input/fungi_2011/'
    #inDir = '/home/salvocos/projects/pyinparanoid/benchmark/input/eukaryote_2011/'
    rootOut = os.path.join(root, 'test_run/')
    cpus = 8
    sens = 4
    coff = 40
    sharedDir = '%sshared_output/'%rootOut
    dbDirectory = '%smmseqs2_databases/'%rootOut
    run_sonicparanoid2(inDir, outDir=rootOut, threads=cpus, sharedDir=sharedDir, mmseqsDbDir=dbDirectory, sensitivity=sens, cutoff=coff, confCutoff=0.05, lenDiffThr=0.5, reuse=True, noMmseqs=False, overwrite=False, debug=debug)



def test_run_sonicparanoid2_multiproc(debug=False):
    """Test a complete sonicparanoid run."""
    local = False

    root = '/home/salvocos/tmp/test_ortholog_detection/'
    #inDir = os.path.join(root, 'input_bigger/')
    #inDir = os.path.join(root, 'input_much_bigger/')
    inDir = '/home/salvocos/projects/pyinparanoid/benchmark/input/qfo_2011/'
    #inDir = '/home/salvocos/projects/pyinparanoid/benchmark/input/eukaryote_2011/'
    #rootOut = os.path.join(root, 'test_run_multiproc/')
    #rootOut = '/tmp/sonicpara_qfo_se4_8x1cpu_parser-fix-minscore30/'
    rootOut = '/tmp/sonicpara_qfo_se4_8x1cpu_parser-fix/'
    cpus = 8
    sens = 2
    coff = 40
    sharedDir = '%sshared_output/'%rootOut
    dbDirectory = '%smmseqs2_databases/'%rootOut
    ov_all = False
    ov_tbls = False

    # set different paths and setting when running locally
    if local:
        root = '/Users/salvocos/Desktop/test_queues_sonicpara/'
        inDir = os.path.join(root, 'input/')
        rootOut = os.path.join(root, 'test_run/')
        cpus = 4
        sens = 4
        coff = 40
        sharedDir = '%sshared_output/'%rootOut
        dbDirectory = '%smmseqs2_databases/'%rootOut
        ov_all = False
        ov_tbls = False

    run_sonicparanoid2_multiproc(inDir, outDir=rootOut, threads=cpus, sharedDir=None, mmseqsDbDir=dbDirectory, sensitivity=sens, cutoff=coff, confCutoff=0.05, lenDiffThr=0.5, noMmseqs=False, overwrite_all=ov_all, overwrite_tbls=ov_tbls, debug=debug)
