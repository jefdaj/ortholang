"""
 Contains function that will process queued jobs, among which alignments,
 and orthology inference.
"""

import os
import sys
import time
import subprocess
import multiprocessing as mp
import queue
import gc
import shutil
from collections import OrderedDict
#### IMPORT TO GENERATE PyPi package
# Import the module for orthology prediction
#'''
from sonicparanoid import inpyranoid
from sonicparanoid import sys_tools as systools
from sonicparanoid import colored_output as colout
#'''
####

#### IMPORTS TO RUN LOCALLY
'''
# Import the module for orthology prediction
import inpyranoid
import sys_tools as systools
import colored_output as colout
#'''
####



__module_name__ = 'Workers'
__source__ = 'workers.py'
__author__ = 'Salvatore Cosentino'
#__copyright__ = ''
__license__ = 'GPL'
__version__ = '1.05'
__maintainer__ = 'Cosentino Salvatore'
__email__ = 'salvo981@gmail.com'


# make a copy of the current environment

# Fix LD_LIBRARY_PATH when using pyinstaller
my_env = os.environ.copy()
if getattr(sys, 'frozen', False):
    if 'LD_LIBRARY_PATH_ORIG' in my_env:
        my_env['LD_LIBRARY_PATH'] = my_env['LD_LIBRARY_PATH_ORIG']
    else:
        my_env['LD_LIBRARY_PATH'] = ''
    if 'DYLD_LIBRARY_PATH_ORIG' in my_env:
        my_env['DYLD_LIBRARY_PATH'] = my_env['DYLD_LIBRARY_PATH_ORIG']
    else:
        my_env['DYLD_LIBRARY_PATH'] = ''



def info():
    """
    Contains function that will process queued jobs, among which alignments,
    and orthology inference.
    """
    print('MODULE NAME:\t%s'%__module_name__)
    print('SOURCE FILE NAME:\t%s'%__source__)
    print('MODULE VERSION:\t%s'%__version__)
    print('LICENSE:\t%s'%__license__)
    print('AUTHOR:\t%s'%__author__)
    print('EMAIL:\t%s'%__email__)



def check_storage_for_mmseqs_dbs(outDir, reqSp=2, gbPerSpecies=1.0, debug=False):
    """Check that there is enough storage for the MMseqs2 index files."""
    if debug:
        print('\ncheck_storage_for_mmseqs_dbs :: START')
        print('Output directory: {:s}'.format(outDir))
        print('Number of databases to be created: {:d}'.format(reqSp))
        print('Required storage for index files: {:0.2f} gigabytes'.format(reqSp * gbPerSpecies))
    availSpaceGb = round(shutil.disk_usage(outDir).free / 1024 ** 3, 2)
    requiredSpaceGb = round(reqSp * gbPerSpecies, 2)
    # set the output variable
    createIdxFiles = True
    if requiredSpaceGb >= availSpaceGb:
        createIdxFiles = False
        infoLn = '{:0.2f} gigabytes required to store the index files for MMseqs2.'.format(requiredSpaceGb)
        colout.colored_info(outLn=infoLn, lnType='i', debug=debug)
        infoLn = 'only {:0.2f} gigabytes avaliable, MMseqs2 index files will not be created.'.format(availSpaceGb)
        colout.colored_info(outLn=infoLn, lnType='w', debug=debug)
        print('Please consider freeing some disk space to take advantage of MMseqs2 index files.')
    if debug:
        print('Available storage in your system (Gigabytes): {:0.2f}'.format(availSpaceGb))
    #sys.exit('DEBUG :: check_storage_for_mmseqs_dbs')
    # return the boolean
    return createIdxFiles



def create_dbs_parallel(jobs_queue, results_queue, inDir, dbDir, create_idx=True):
    """Create a a mmseqs2 database for the species in input dir."""
    while True:
        current_sp = jobs_queue.get(True, 1)
        if current_sp is None:
            break
        # check the query db name
        #queryDBname = os.path.basename(inFasta)
        inQueryPath = os.path.join(inDir, current_sp)
        if not os.path.isfile(inQueryPath):
            print('ERROR: Input FASTA file {:s} not found\n'.format(inQueryPath))
        queryDBname = '{:s}.mmseqs2db'.format(current_sp)
        queryDBpath = '{:s}{:s}'.format(dbDir, queryDBname)
        # create the database if does not exist yet
        if not os.path.isfile(queryDBpath):
            start_time = time.perf_counter()
            mmseqs_createdb(inQueryPath, outDir=dbDir, debug=False)
            if create_idx:
                mmseqs_createindex(queryDBpath, debug=False)
            end_time = time.perf_counter()
            # add the execution time to the results queue
            results_queue.put((current_sp, str(round(end_time - start_time, 2))))



def consume_mmseqs_search_1cpu(jobs_queue, results_queue, inDir, outDir, dbDir, keepAlign, sensitivity, cutoff, pPath):
    while True:
        current_pair = jobs_queue.get(True, 1)
        if current_pair is None:
            break
        # extract input paths
        sp1, sp2 = current_pair.split('-', 1)
        inSeq = os.path.join(inDir, sp1)
        dbSeq = os.path.join(inDir, sp2)
        # create temporary directory name
        tmpMMseqsDirName = 'tmp_{:s}-{:s}'.format(sp1, sp2)
        # it MUST use 1 CPU
        parsedOutput, search_time, convert_time, parse_time, tot_time = mmseqs_1pass(inSeq, dbSeq, dbDir=dbDir, outDir=outDir, tmpDirName=tmpMMseqsDirName, keepAlign=keepAlign, sensitivity=sensitivity, evalue=1000, cutoff=cutoff, threads=1, pythonPath=pPath, debug=False)
        # exit if the BLAST for,atted file generation was not successful
        if not os.path.isfile(parsedOutput):
            sys.stderr.write('\nERROR: the MMseqs2 raw alignment could not be converted inot the BLAST alignment format.\n')
            sys.exit(-2)

        #print('{:s}\t{:s}\t{:s}\t{:s}\t{:s}'.format(current_pair, str(search_time), str(convert_time), str(parse_time), str(tot_time)))
        # store the execution time in the queue
        results_queue.put((current_pair, search_time, convert_time, parse_time))



def consume_orthology_inference_jobs_sharedict_jobs(jobs_queue, results_queue, inDir, outDir=os.getcwd(), sharedDir=None, sharedWithinDict=None, cutoff=40, confCutoff=0.05, lenDiffThr=0.5, threads=8, debug=False):
    """Perform orthology inference in parallel."""
    while True:
        current_pair = jobs_queue.get(True, 1)
        if current_pair is None:
            break
        # create the output directory if needed
        # prepare the run
        sp1, sp2 = current_pair.split('-', 1)
        runDir = os.path.join(outDir, current_pair)
        systools.makedir(runDir)
        inSp1 = os.path.join(inDir, sp1)
        inSp2 = os.path.join(inDir, sp2)
        # check that the input files do exist
        if not os.path.isfile(inSp1):
            sys.stderr.write('ERROR: The input file for {:s} was not found, please provide a valid path\n.'.format(sp1))
        if not os.path.isfile(inSp2):
            sys.stderr.write('ERROR: The input file for {:s} was not found, please provide a valid path\n.'.format(sp2))
        # prepare the names of the required alignments
        # AB
        AB = '{:s}-{:s}'.format(sp1, sp2)
        shPathAB = os.path.join(sharedDir, AB)
        if not os.path.isfile(shPathAB):
            sys.stderr.write('ERROR: The alignment file for {:s} was not found, please generate alignments first\n.'.format(AB))
        # BA
        BA = '{:s}-{:s}'.format(sp2, sp1)
        shPathBA = os.path.join(sharedDir, BA)
        if not os.path.isfile(shPathBA):
            sys.stderr.write('ERROR: The alignment file for {:s} was not found, please generate alignments first\n.'.format(BA))
        #sys.exit('DEBUG :: workers :: consume_orthology_inference_jobs_sharedict_jobs :: all-vs-all exstance check')

        # prepare paths for output tables
        outTable = os.path.join(runDir, 'table.{:s}'.format(current_pair))
        outSql = os.path.join(runDir, 'sqltable.{:s}'.format(current_pair))

        # infer orthologs
        # use perf_counter (includes time spent during sleep)
        orthology_prediction_start = time.perf_counter()
        inpyranoid.infer_orthologs_shared_dict(inSp1, inSp2, alignDir=sharedDir, outDir=runDir, sharedWithinDict=sharedWithinDict, confCutoff=confCutoff, lenDiffThr=lenDiffThr, debug=False)

        #check that all the files have been created
        if not os.path.isfile(outTable):
            sys.stderr.write('WARNING: the ortholog table file %s was not generated.'%outTable)
            outTable = None
        if not os.path.isfile(outSql):
            sys.stderr.write('WARNING: the SQL table %s was not generated.'%outSql)
            outSql = None
        #everything went ok!
        end_time = time.perf_counter()
        orthology_prediction_tot = round(end_time - orthology_prediction_start, 2)
        #sys.exit('DEBUG :: workers :: consume_orthology_inference_jobs_sharedict_jobs :: orthology done!')
        # add the execution time to the results queue
        results_queue.put((current_pair, str(orthology_prediction_tot)))
        if debug:
            sys.stdout.write('\nOrthology prediction {:s} (seconds):\t{:s}\n'.format(current_pair, str(orthology_prediction_tot)))



def consume_orthology_inference_jobs(jobs_queue, results_queue, inDir, outDir=os.getcwd(), sharedDir=None, cutoff=40, confCutoff=0.05, lenDiffThr=0.5, threads=8, debug=False):
    """Perform orthology inference in parallel."""
    while True:
        current_pair = jobs_queue.get(True, 1)
        if current_pair is None:
            break
        # create the output directory iof needed
        # prepare the run
        sp1, sp2 = current_pair.split('-', 1)
        runDir = os.path.join(outDir, current_pair)
        systools.makedir(runDir)
        inSp1 = os.path.join(inDir, sp1)
        inSp2 = os.path.join(inDir, sp2)
        # check that the input files do exist
        if not os.path.isfile(inSp1):
            sys.stderr.write('ERROR: The input file for {:s} was not found, please provide a valid path\n.'.format(sp1))
        if not os.path.isfile(inSp2):
            sys.stderr.write('ERROR: The input file for {:s} was not found, please provide a valid path\n.'.format(sp2))
        # prepare the names of the required alignments
        # copy AA
        AA = '{:s}-{:s}'.format(sp1, sp1)
        shPathAA = os.path.join(sharedDir, AA)
        if not os.path.isfile(shPathAA):
            sys.stderr.write('ERROR: The alignment file for {:s} was not found, please generate alignments first\n.'.format(AA))
        # copy BB
        BB = '{:s}-{:s}'.format(sp2, sp2)
        shPathBB = os.path.join(sharedDir, BB)
        if not os.path.isfile(shPathBB):
            sys.stderr.write('ERROR: The alignment file for {:s} was not found, please generate alignments first\n.'.format(BB))
        # copy AB
        AB = '{:s}-{:s}'.format(sp1, sp2)
        shPathAB = os.path.join(sharedDir, AB)
        if not os.path.isfile(shPathAB):
            sys.stderr.write('ERROR: The alignment file for {:s} was not found, please generate alignments first\n.'.format(AB))
        # copy BA
        BA = '{:s}-{:s}'.format(sp2, sp1)
        shPathBA = os.path.join(sharedDir, BA)
        if not os.path.isfile(shPathBA):
            sys.stderr.write('ERROR: The alignment file for {:s} was not found, please generate alignments first\n.'.format(BA))
        #sys.exit('DEBUG :: workers :: consume_orthology_inference_jobs :: after files copy')

        # prepare paths for output tables
        outTable = os.path.join(runDir, 'table.{:s}'.format(current_pair))
        outSql = os.path.join(runDir, 'sqltable.{:s}'.format(current_pair))

        # infer orthologs
        # use perf_counter (includes time spent during sleep)
        orthology_prediction_start = time.perf_counter()
        inpyranoid.infer_orthologs(inSp1, inSp2, alignDir=sharedDir, outDir=runDir, confCutoff=confCutoff, lenDiffThr=lenDiffThr, debug=False)
        #sys.exit('DEBUG :: workers :: consume_orthology_inference_jobs :: after orthology')
        #check that all the files have been created
        if not os.path.isfile(outTable):
            sys.stderr.write('WARNING: the ortholog table file %s was not generated.'%outTable)
            outTable = None
        if not os.path.isfile(outSql):
            sys.stderr.write('WARNING: the SQL table %s was not generated.'%outSql)
            outSql = None
        #everything went ok!
        # use perf_counter (includes time spent during sleep)
        end_time = time.perf_counter()
        orthology_prediction_tot = round(end_time - orthology_prediction_start, 2)
        #sys.exit('DEBUG :: workers :: consume_orthology_inference_jobs :: orthology done!')
        # add the execution time to the results queue
        results_queue.put((current_pair, str(orthology_prediction_tot)))
        if debug:
            sys.stdout.write('\nOrthology prediction {:s} (seconds):\t{:s}\n'.format(current_pair, str(orthology_prediction_tot)))



def consume_write_seq_lengths(jobs_queue, results_queue, outDir):
    """Write sequence lengths in a file."""
    while True:
        current_input = jobs_queue.get(True, 1)
        if current_input is None:
            break
        # extract species name
        sp = os.path.basename(current_input)
        out_path = os.path.join(outDir, '{:s}.len'.format(sp))
        # create file with sequence lengths
        genome_size = write_seq_lengths(current_input, out_path, debug=False)
        results_queue.put((sp, genome_size))



def get_mmseqs_path():
    """Return the directory in which MMseqs2 binaries are stored."""
    #import platform
    mmseqsPath = None
    pySrcDir = os.path.dirname(os.path.abspath(__file__))
    # Config file paths
    '''
    cfgPath = os.path.join(pySrcDir, 'config.json')
    if not os.path.isfile(cfgPath):
        sys.stderr.write('\nERROR: SonicParanoid has not been setup yet, please run setup_sonicparanoid.py.')
    '''
    # check the OS type
    '''
    ostype = platform.system()
    if ostype == 'Darwin':
        mmseqsPath = os.path.join(pySrcDir, 'bin/osx/mmseqs')
    elif ostype == 'Linux':
        mmseqsPath = os.path.join(pySrcDir, 'bin/linux/mmseqs')
    '''
    mmseqsPath = os.path.join(pySrcDir, 'bin/mmseqs')
    if not os.path.isfile(mmseqsPath):
        sys.stderr.write('\nERROR: mmseqs2 was not found, please install it and execute setup_sonicparanoid.py.')
    # return the path
    return mmseqsPath



def mmseqs_1pass(inSeq, dbSeq, dbDir=os.getcwd(), outDir=os.getcwd(), tmpDirName=None, keepAlign=False, sensitivity=4.0, evalue=1000, cutoff=40, threads=4, pythonPath=sys.executable, debug=False):
    """Execute the 1-pass alignment mmseqs2 similar to the one implemented in core-inparanoid."""
    if debug:
        print('\nmmseqs_1pass :: START')
        print('Input query FASTA file:\t%s'%inSeq)
        print('Input target FASTA file:\t%s'%dbSeq)
        print('mmseqs2 database directory:\t%s'%dbDir)
        print('Output directory:\t%s'%outDir)
        print('MMseqs2 tmp directory:\t{:s}'.format(tmpDirName))
        print('Do not remove alignment files:\t{:s}'.format(str(keepAlign)))
        print('MMseqs2 sensitivity (-s):\t%s'%str(sensitivity))
        print('Bitscore cutoff:\t%d'%cutoff)
        print('Threads:\t%d'%threads)
        print('Python3 binaries for parsing:\t{:s}'.format(pythonPath))
    #start the timing which will also include the time for the index, database creation (if required) and parsing
    # create mmseqs alignment conveted into blastp tab-separated format
    blastLikeOutput, search_time, convert_time = mmseqs_search(inSeq, dbSeq, dbDir=dbDir, outDir=outDir, tmpDirName=tmpDirName, sensitivity=sensitivity, evalue=1000, threads=threads, cleanUp=False, debug=debug)
    parserPath = '%smmseqs_parser_cython.py'%outDir
    prevDir = os.getcwd()
    os.chdir(outDir)
    # check cutoff
    if cutoff < 30:
        cutoff = 40
    # start timing the parsing
    #start_time = time.time()
    # use perf_counter (includes time spent during sleep)
    start_time = time.perf_counter()
    # use process_time (no sleep time)
    #start_time = time.process_time()
    # prepare now the parsing
    # EXAMPLE: python3 mmseqs_parser_cython.py --input mmseqs2blast.A-B --query A --db B --output A-B --cutoff 40
    parsedOutput = blastLikeOutput.replace('mmseqs2blast.', '')
    # READ LENGTHS OR HDRS FROM FILE
    parseCmd = '{:s} {:s} --input {:s} --query {:s} --db {:s} --output {:s} --cutoff {:d}'.format(pythonPath, parserPath, blastLikeOutput, inSeq, dbSeq, parsedOutput, cutoff)
    # LENGTHS ALREADY INCLUDED IN MMSEQS OUTPUT
    #parseCmd = 'python3 %s --input %s --output %s --cutoff %d'%(parserPath, blastLikeOutput, parsedOutput, cutoff)
    if debug:
        print('Parse CMD:\n{:s}'.format(parseCmd))
    #execute the system call for parsing
    subprocess.run(parseCmd, env=my_env, shell=True)
    # use perf_time (includes time spent during sleep)
    parse_time = round(time.perf_counter() - start_time, 2)
    tot_time = round(search_time + convert_time + parse_time, 2)

    if debug:
        sys.stdout.write('\nMMseqs2 alignment and parsing elapsed time (seconds):\t%s\n'%str(tot_time))
    # now it possible to remove the temporary files...
    mmseqs_cleanup(inDir=outDir, tmpDirPath=os.path.join(outDir, tmpDirName), tmpOnly=keepAlign, debug=debug)
    # reset original working directory
    os.chdir(prevDir)
    return (parsedOutput, search_time, convert_time, parse_time, tot_time)



def mmseqs_cleanup(inDir=os.getcwd(), tmpDirPath=None, tmpOnly=False, debug=False):
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
    if not tmpOnly:
        for f in os.listdir(inDir):
            if fnmatch.fnmatch(f, 'mmseqs2*.{:s}*'.format(alignName)):
                os.remove('%s%s'%(inDir, f))
    if os.path.isdir(tmpDirPath):
        shutil.rmtree(tmpDirPath)



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
    makeDbCmd = '%s createdb %s %s -v 0'%(get_mmseqs_path(), inSeq, dbPath)
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
    #''' USE IN FUTURE VERSION OF CREATEINDEX
    #print(dbPath)
    tmpBname = os.path.basename(dbPath)
    tmpDir = '{:s}/tmp_{:s}/'.format(os.path.dirname(dbPath), os.path.basename(tmpBname.split('.', 1)[0]))
    systools.makedir(tmpDir)
    makeIdxCmd = '{:s} createindex {:s} {:s} --threads 2 -v 0'.format(get_mmseqs_path(), dbPath, tmpDir)
    #'''
    # command to be executed
    # EXAMPLE; mmseqs createindex in.mmseqs2_db
    #makeIdxCmd = '{:s} createindex {:s} -v 0'.format(get_mmseqs_path(), dbPath)
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
        sys.stderr.write('The MMseqs2 index file %s could not be created.'%idx1)
        sys.exit(-2)
    idx2 = '%s.sk6.index'%dbPath
    if not os.path.isfile(idx2):
        sys.stderr.write('\nWARNING: The MMseqs2 index file %s could not be created.'%idx2)
        #sys.exit(-2)
    # remove the temporary directory
    shutil.rmtree(path=tmpDir)
    # return a output tuple
    return(stdout_val, stderr_val, makeIdxCmd, idx1, idx2)



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
        mmseqs_createindex(queryDBpath, debug=debug)
    # check the target db name
    targetDBname = os.path.basename(dbSeq)
    targetDBname = targetDBname.split('.')[0] # take the left part of the file name
    targetDBname = '%s.mmseqs2db'%targetDBname
    targetDBpath = '%s%s'%(dbDir, targetDBname)
    # create the database if does not exist yet
    if not os.path.isfile(targetDBpath):
        mmseqs_createdb(dbSeq, outDir=dbDir, debug=debug)
        mmseqs_createindex(targetDBpath, debug=debug)
    # set output name
    pairName = '%s-%s'%(os.path.basename(inSeq), os.path.basename(dbSeq))
    rawOutName = 'mmseqs2raw.%s'%pairName
    rawOutPath = '%s%s'%(outDir, rawOutName)
    blastOutName = 'mmseqs2blast.%s'%pairName
    blastOutPath = '%s%s'%(outDir, blastOutName)
    # start measuring the execution time
    # use perf_counter (includes time spent during sleep)
    start_time = time.perf_counter()
    # command to be executed
    minUngappedScore = 15
    # EXAMPLE; mmseqs search queryDBfile targetDBfile outputFile tmpDir -s 7.5 -e 100000 --theads threads
    searchCmd = '{:s} search {:s} {:s} {:s} {:s} -s {:s} --threads {:d} -v 0 --min-ungapped-score {:d} --alignment-mode 2 --alt-ali 10'.format(get_mmseqs_path(), queryDBpath, targetDBpath, rawOutPath, tmpDir, str(sensitivity), threads, minUngappedScore)
    if debug:
        print('mmseqs2 search CMD:\t%s'%searchCmd)
    # use run (or call)
    subprocess.run(searchCmd, env=my_env, shell=True)
    # output an error if the Alignment did not finish correctly
    if not os.path.isfile(rawOutPath):
        sys.stderr.write('\nERROR: the MMseqs2 raw alignment file was not generated.\n')
        sys.exit(-2)
    # stop counter
    # use perf_counter (includes time spent during sleep)
    end_search = time.perf_counter()
    # use process_time (user + system CPU time, no sleep time)
    #end_search = time.process_time()
    search_time = round(end_search - start_time, 2)
    # convert the output to tab-separated BLAST output
    # EXAMPLE: mmseqs convertalis query.db target.db query_target_rawout query_target_blastout
    convertCmd = '%s convertalis %s %s %s %s -v 0 --format-mode 0'%(get_mmseqs_path(), queryDBpath, targetDBpath, rawOutPath, blastOutPath)
    # perform the file conversion
    subprocess.run(convertCmd, env=my_env, shell=True)
    if debug:
        print('mmseqs2 convertalis CMD:\t%s'%convertCmd)
    # exec time conversion
    #convert_time = round(time.time() - end_search, 2)
    # use perf_counter (includes time spent during sleep)
    convert_time = round(time.perf_counter() - end_search, 2)
    # use process_time (user + system CPU time, no sleep time)
    #convert_time = round(time.process_time() - end_search, 2)
    # cleanup output directory
    if cleanUp:
        mmseqs_cleanup(inDir=outDir, debug=debug)
    # output an error if the Alignment could not be converted
    if not os.path.isfile(blastOutPath):
        sys.stderr.write('\nERROR: the MMseqs2 raw alignment could not be converted inot the BLAST alignment format.\n')
        sys.exit(-2)
    return (blastOutPath, search_time, convert_time)



def perform_parallel_dbs_creation(spList, inDir, dbDir, create_idx=True, threads=4, debug=False):
    """Create MMseqs2 databases in parallel"""
    # create the queue and start adding
    make_dbs_queue = mp.Queue(maxsize=len(spList) + threads)

    # fill the queue with the processes
    for sp in spList:
        sys.stdout.flush()
        make_dbs_queue.put(os.path.basename(sp))

    # add flags for eneded jobs
    for i in range(0, threads):
        sys.stdout.flush()
        make_dbs_queue.put(None)

    # Queue to contain the execution time
    results_queue = mp.Queue(maxsize=len(spList))

    # call the method inside workers
    runningJobs = [mp.Process(target=create_dbs_parallel, args=(make_dbs_queue, results_queue, inDir, dbDir, create_idx)) for i_ in range(threads)]

    for proc in runningJobs:
        proc.start()

    while True:
        try:
            sp, tot_time = results_queue.get(False, 0.01)
            #ofd.write('{:s}\t{:s}\t{:s}\t{:s}\n'.format(p, str(s_time), str(c_time), str(p_time)))
            if debug:
                sys.stdout.write('{:s} database created:\t{:s}\n'.format(sp, tot_time))
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



def perform_mmseqs_multiproc_alignments(required_align, inDir, outDir, dbDir, create_idx=True, sensitivity=6, cutoff=0.5, threads=4, keepAlign=False, debug=False):
    system_cpus = mp.cpu_count()
    # check threads count
    if threads > system_cpus:
        threads = system_cpus

    # create dictionary with species involved in alignments
    reqSpDict = {}
    for pair in required_align:
        sp1, sp2 = pair.split('-', 1)
        if not sp1 in reqSpDict:
            reqSpDict[sp1] = None
        if not sp2 in reqSpDict:
            reqSpDict[sp2] = None

    # Make sure there is enough storage to crate the index files
    # overwrites the create_idx variable
    if create_idx:
        create_idx = check_storage_for_mmseqs_dbs(outDir, reqSp=len(reqSpDict), gbPerSpecies=1, debug=debug)

    # create databases
    sys.stdout.write('\nCreating {:d} MMseqs2 databases...\n'.format(len(reqSpDict)))
    # timer for databases creation
    start_time = time.perf_counter()

    # create databases in parallel
    perform_parallel_dbs_creation(list(reqSpDict.keys()), inDir, dbDir, create_idx=create_idx, threads=threads, debug=debug)
    # end time for databases creation
    end_time = time.perf_counter()
    sys.stdout.write('\nMMseqs2 databases creation elapsed time (seconds):\t{:s}\n'.format(str(round(end_time - start_time, 3))))
    #sys.exit('DEBUG :: workers :: perform_mmseqs_multiproc_alignments')
    # delete timers
    del start_time, end_time
    # calculate cpu-time for alignments
    align_start = time.perf_counter()
    # find the mmseqs2 parser in the source directory
    pySrcDir = os.path.dirname(os.path.abspath(__file__))
    mmseqsparser = os.path.join(pySrcDir, 'mmseqs_parser_cython.py')
    # copy the file to the output directory
    systools.copy(mmseqsparser, outDir, metaData=False, debug=False)
    mmseqsparser = os.path.join(outDir, 'mmseqs_parser_cython.py')
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

    # create the queue and start adding
    align_queue = mp.Queue(maxsize=len(required_align) + threads)

    # fill the queue with the processes
    for pair in required_align:
        sys.stdout.flush()
        align_queue.put(pair)

    # add flags for eneded jobs
    for i in range(0, threads):
        sys.stdout.flush()
        align_queue.put(None)

    # Queue to contain the execution time
    results_queue = mp.Queue(maxsize=len(required_align))

    # get the path to python3 executable
    pythonPath = sys.executable
    # call the method inside workers
    runningJobs = [mp.Process(target=consume_mmseqs_search_1cpu, args=(align_queue, results_queue, inDir, outDir, dbDir, keepAlign, sensitivity, cutoff, pythonPath)) for i_ in range(threads)]

    for proc in runningJobs:
        proc.start()

    # open the file in which the time information will be stored
    # use the parent directory name of the database directory as suffix
    execTimeOutPath = os.path.join(outDir, 'alignments_ex_time_{:s}.tsv'.format(dbDir.rstrip('/').rsplit('/')[-2]))
    ofd = open(execTimeOutPath, 'w', buffering=1)

    # write some message...
    sys.stdout.write('\nPerforming the required {:d} MMseqs2 alignments...'.format(len(required_align)))
    # write output when available
    while True:
        try:
            p, s_time, c_time, p_time = results_queue.get(False, 0.01)
            ofd.write('{:s}\t{:s}\t{:s}\t{:s}\n'.format(p, str(s_time), str(c_time), str(p_time)))
        except queue.Empty:
            pass
        allExited = True
        for t in runningJobs:
            if t.exitcode is None:
                allExited = False
                break
        if allExited & results_queue.empty():
            break
    ofd.close()

    # this joins the processes after we got the results
    for proc in runningJobs:
        while proc.is_alive():
            proc.join()

    # stop the counter for the alignment time
    sys.stdout.write('\nAll-vs-all alignments elapsed time (seconds):\t{:s}\n'.format(str(round(time.perf_counter() - align_start, 3))))
    #sys.exit('DEBUG :: workers :: perform_mmseqs_multiproc_alignments :: all alignments done')



def perform_parallel_orthology_inference(requiredPairsDict, inDir, outDir=os.getcwd(), sharedDir=None, cutoff=40, confCutoff=0.05, lenDiffThr=0.5, threads=8, debug=False):
    """Execute orthology inference for the required pairs."""
    if debug:
        print('\nperform_parallel_orthology_inference :: START')
        print('Proteome pairs to be processed:\t{:d}'.format(len(requiredPairsDict)))
        print('Input directory:{:s}'.format(inDir))
        print('Outdir:{:s}'.format(outDir))
        print('Alignment directory:{:s}'.format(sharedDir))
        print('Cutoff:\t{:d}'.format(cutoff))
        print('Confidence cutoff for paralogs:\t{:s}'.format(str(confCutoff)))
        print('Length difference filtering threshold:\t{:s}'.format(str(lenDiffThr)))
        print('CPUs (for mmseqs):\t{:d}'.format(threads))
    # make sure that the directory with alignments exists
    if not os.path.isdir(sharedDir):
        sys.stderr.write('ERROR: The directory with the alignment files\n%s\nwas not found, please provide a valid path\n'.format(sharedDir))
    if not os.path.isdir(sharedDir):
        sys.stderr.write('ERROR: The directory with the input files\n{:s}\nwas not found, please provide a valid path\n'.format(inDir))
    #create the output directory if does not exist yet
    if outDir != os.getcwd():
        if not os.path.isdir(outDir):
            systools.makedir(outDir)
    if outDir[-1] != '/':
        outDir += '/'
    # check if the output directory differs from the input one
    if os.path.dirname(inDir) == os.path.dirname(outDir):
        sys.stderr.write('\nERROR: the output directory {:s}\nmust be different from the one in which the input files are stored.\n'.format(outDir))
        sys.exit(-2)
    # check cutoff
    if cutoff < 30:
        cutoff = 40
    # create the queue and start adding the jobs
    jobs_queue = mp.Queue()

    # fill the queue with the processes
    for pair in requiredPairsDict:
        jobs_queue.put(pair)
    # add flags for eneded jobs
    for i in range(0, threads):
        jobs_queue.put(None)
    #sys.exit('DEBUG :: 3')

    # Queue to contain the execution time
    results_queue = mp.Queue(maxsize=len(requiredPairsDict))
    # call the method inside workers
    runningJobs = [mp.Process(target=consume_orthology_inference_jobs, args=(jobs_queue, results_queue, inDir, outDir, sharedDir, cutoff, confCutoff, lenDiffThr, threads, debug)) for i_ in range(threads)]

    for proc in runningJobs:
        #print('Start job\t{}'.format(proc))
        proc.start()

    # open the file in which the time information will be stored
    execTimeOutPath = os.path.join(sharedDir, 'orthology_ex_time_{:s}.tsv'.format(os.path.basename(outDir.rstrip('/'))))
    ofd = open(execTimeOutPath, 'w', buffering=1)

    # get the results from the queue without filling the Pipe buffer
    while True:
        try:
            p, val = results_queue.get(False, 0.01)
            ofd.write('{:s}\t{:s}\n'.format(p, str(val)))
        except queue.Empty:
            pass
        allExited = True
        for t in runningJobs:
            if t.exitcode is None:
                allExited = False
                break
        if allExited & results_queue.empty():
            break
    ofd.close()

    for proc in runningJobs:
        while proc.is_alive():
            proc.join()



def perform_parallel_orthology_inference_shared_dict(requiredPairsDict, inDir, outDir=os.getcwd(), sharedDir=None, sharedWithinDict=None, cutoff=40, confCutoff=0.05, lenDiffThr=0.5, threads=8, debug=False):
    """Execute orthology inference for the required pairs."""
    if debug:
        print('\nperform_parallel_orthology_inference_shared_dict :: START')
        print('Proteome pairs to be processed:\t{:d}'.format(len(requiredPairsDict)))
        print('Input directory:{:s}'.format(inDir))
        print('Outdir:{:s}'.format(outDir))
        print('Alignment directory:{:s}'.format(sharedDir))
        print('Shared within-align dictionaries :{:d}'.format(len(sharedWithinDict)))
        print('Cutoff:\t{:d}'.format(cutoff))
        print('Confidence cutoff for paralogs:\t{:s}'.format(str(confCutoff)))
        print('Length difference filtering threshold:\t{:s}'.format(str(lenDiffThr)))
        print('CPUs (for mmseqs):\t{:d}'.format(threads))
    # make sure that the directory with alignments exists
    if not os.path.isdir(sharedDir):
        sys.stderr.write('ERROR: The directory with the alignment files\n%s\nwas not found, please provide a valid path\n'.format(sharedDir))
    if not os.path.isdir(sharedDir):
        sys.stderr.write('ERROR: The directory with the input files\n{:s}\nwas not found, please provide a valid path\n'.format(inDir))
    #create the output directory if does not exist yet
    if outDir != os.getcwd():
        if not os.path.isdir(outDir):
            systools.makedir(outDir)
    if outDir[-1] != '/':
        outDir += '/'
    # check if the output directory differs from the input one
    if os.path.dirname(inDir) == os.path.dirname(outDir):
        sys.stderr.write('\nERROR: the output directory {:s}\nmust be different from the one in which the input files are stored.\n'.format(outDir))
        sys.exit(-2)
    # check cutoff
    if cutoff < 30:
        cutoff = 40
    # create the queue and start adding the jobs
    jobs_queue = mp.Queue()

    # fill the queue with the processes
    for pair in requiredPairsDict:
        jobs_queue.put(pair)
    # add flags for eneded jobs
    for i in range(0, threads):
        jobs_queue.put(None)

    #sys.exit('\nDEBUG :: workers :: perform_parallel_orthology_inference_shared_dict')

    # Queue to contain the execution time
    results_queue = mp.Queue(maxsize=len(requiredPairsDict))
    # call the method inside workers
    runningJobs = [mp.Process(target=consume_orthology_inference_jobs_sharedict_jobs, args=(jobs_queue, results_queue, inDir, outDir, sharedDir, sharedWithinDict, cutoff, confCutoff, lenDiffThr, threads, debug)) for i_ in range(threads)]

    for proc in runningJobs:
        #print('Start job\t{}'.format(proc))
        proc.start()

    # open the file in which the time information will be stored
    execTimeOutPath = os.path.join(sharedDir, 'orthology_ex_time_{:s}.tsv'.format(os.path.basename(outDir.rstrip('/'))))
    ofd = open(execTimeOutPath, 'w', buffering=1)
    # update the shared dictionary
    # and remove the shared dictionary if required

    # get the results from the queue without filling the Pipe buffer
    while True:
        try:
            p, val = results_queue.get(False, 0.01)
            ofd.write('{:s}\t{:s}\n'.format(p, str(val)))
            #'''
            sp1, sp2 = p.split('-', 1)
            # decrease the counters in the shared dictionaries
            sharedWithinDict[sp1][0] -= 1
            if sharedWithinDict[sp1][0] == 0:
                del sharedWithinDict[sp1]
                # call the garbage collector to free memory explicitly
                gc.collect()
                if debug:
                    print('Removed dictionary for {:s}'.format(sp1))
                    print('Remaining shared dictionaries:\t{:d}'.format(len(sharedWithinDict)))
            sharedWithinDict[sp2][0] -= 1
            if sharedWithinDict[sp2][0] == 0:
                del sharedWithinDict[sp2]
                if debug:
                    print('Removed dictionary for {:s}'.format(sp2))
                    print('Remaining shared dictionaries:\t{:d}'.format(len(sharedWithinDict)))

        except queue.Empty:
            pass
        allExited = True
        for t in runningJobs:
            if t.exitcode is None:
                allExited = False
                break
        if allExited & results_queue.empty():
            break
    ofd.close()

    for proc in runningJobs:
        while proc.is_alive():
            proc.join()
    #sys.exit('\nDEBUG :: workers :: perform_parallel_orthology_inference_shared_dict :: End!')



def write_seq_len_files_parallel(inDir, outDir, threads=4, debug=False):
    '''Create files with sequence lengths in parallel.'''
    if debug:
        print('write_seq_len_files_parallel :: START')
        print('Input directory:\t{:s}'.format(inDir))
        print('Number of threads:\t{:d}'.format(threads))
    # input paths
    pathsList = []
    # list the input files
    for f in os.listdir(inDir):
        if f.startswith('.DS_Store'):
            continue
        # check existance of input file
        tmpPath = os.path.join(inDir, f)
        if not os.path.isfile(tmpPath):
            print('ERROR: Input FASTA file {:s} not found\n'.format(tmpPath))
        # add path to the dictionary
        pathsList.append(tmpPath)

    # will associate a genome size to each species
    spSizeDict = {}
    # create the queue and start adding
    jobs_queue = mp.Queue(maxsize=len(pathsList) + threads)

    # fill the queue with the processes
    for  path in pathsList:
        sys.stdout.flush()
        jobs_queue.put(path)

    # add flags for eneded jobs
    for i in range(0, threads):
        sys.stdout.flush()
        jobs_queue.put(None)

    # Queue to contain the execution time
    results_queue = mp.Queue(maxsize=len(pathsList))

    # call the method inside workers
    runningJobs = [mp.Process(target=consume_write_seq_lengths, args=(jobs_queue, results_queue, outDir)) for i_ in range(threads)]

    for proc in runningJobs:
        proc.start()

    while True:
        try:
            sp, genome_size = results_queue.get(False, 0.01)
            spSizeDict[sp] = genome_size
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

    # return the dictionary with genome sizes
    return spSizeDict



def write_seq_lengths(proteome, out_path, debug=False):
    '''Write the length of each protein sequence in a output file.'''
    if debug:
        print('\nwrite_seq_lengths :: START')
        print('Proteome/Genome:\t{:s}'.format(proteome))
        print('Output file:\t{:s}'.format(out_path))
    # open sequence file
    ofd = open(out_path, 'w')
    seqCnt = 0
    genomeSize = 0
    # write a pkl file with the lengths
    picDict = {}
    picklPath = '{:s}.pkl'.format(out_path)
    # use biopython
    from Bio import SeqIO
    for seq_record in SeqIO.parse(open(proteome), 'fasta'):
        tmpLen = len(seq_record)
        ofd.write('{:s}\t{:d}\n'.format(seq_record.id, tmpLen))
        picDict[seq_record.id] = tmpLen
        seqCnt += 1
        genomeSize += tmpLen
    if debug:
        print('Written sequence lengths for %s:\t%d'%(os.path.basename(proteome), seqCnt))
    ofd.close()
    return genomeSize
