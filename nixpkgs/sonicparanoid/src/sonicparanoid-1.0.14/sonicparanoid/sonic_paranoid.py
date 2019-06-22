# -*- coding: utf-8 -*-
'''Execute the SonicParanoid.'''
import os
import sys
import platform
import subprocess
#### IMPORT TO GENERATE PyPi package
#'''
from sonicparanoid import seq_tools as seqtools
from sonicparanoid import ortholog_detection as orthodetect
from sonicparanoid import sys_tools as systools
#'''
####

#### IMPORTS TO RUN LOCALLY
'''
import seq_tools as seqtools
import ortholog_detection as orthodetect
import sys_tools as systools
#'''
####

import fnmatch
import numpy as np
import time

########### FUNCTIONS ############
def get_params():
    """Parse and analyse command line parameters."""
    # create the possible values for sensitivity value
    cnt = 0.9
    sensList = []
    for i in range(1, 9):
        sensList.append(float(i))
        for j in range(1, 10):
            fval = float(i + float(j / 10.))
            if fval <= 8.5:
                sensList.append(fval)
    # define the parameter list
    import argparse
    sonicparanoid_usage = '\nDefine --input-directory if you want to find orthologs for (at least 3) species in the input directory.\n'
    parser = argparse.ArgumentParser(description='SonicParanoid 1.0',  usage='%(prog)s -i <INPUT_DIRECTORY> -o <OUTPUT_DIRECTORY>[options]', prog='sonicparanoid')
    #start adding the command line options
    parser.add_argument('-i', '--input-directory', type=str, required=True, help='Directory containing the proteomes (in FASTA format) of the species to be compared. NOTE: The file names MUST NOT contain the \'-\' nor \'.\' characters', default=None)
    parser.add_argument('-o', '--output-directory', type=str, required=True, help='The directory in which the results will be stored.', default=None)
    parser.add_argument('-sh', '--shared-directory', type=str, required=False, help='The directory in which the alignment files are stored. If not specified it will be created inside the main output directory.', default=None)
    parser.add_argument('-db', '--mmseqs-dbs', type=str, required=False, help='The directory in which the database files for MMseqs2 will be stored. If not specified it will be created inside the main output directory.', default=None)
    parser.add_argument('-t', '--threads', type=int, required=False, help='Number of parallel 1-CPU jobs to be used. Default=4', default=4)
    #parser.add_argument('-c', '--cutoff', type=int, required=False, help='Cutoff value to be used when processing alignments.', default=40)
    parser.add_argument('-se', '--sensitivity', type=float, required=False, help='Sensitivity for MMseqs2. This will overwrite the --mode option.', default=None)
    parser.add_argument('-m', '--mode', required=False, help='SonicParanoid execution mode. The default mode is suitable for most studies. Use sensitive if the input proteomes are not closely related.', choices=['fast', 'default', 'sensitive', 'most-sensitive'], default='default')
    parser.add_argument('-noidx', '--no-indexing', required=False, help='Avoid the creation of indexes for MMseqs2 databases. IMPORTANT: while this saves starting storage space it makes MMseqs2 slightly slower.\nThe results might also be sligthy different.', default=False, action='store_true')
    parser.add_argument('-ot', '--overwrite-tables', required=False, help='This will force the re-computation of the ortholog tables. Only missing alignment files will be re-computed.', default=False, action='store_true')
    parser.add_argument('-ow', '--overwrite', required=False, help='Overwrite previous runs and execute it again. This can be useful to update a subset of the computed tables.', default=False, action='store_true')
    parser.add_argument('-mo', '--multi-species-only', required=False, help='Use this option if you already have the proteome-proteome ortholog tables from a previous execution of SonicParanoid, and you only want to perform multi-species orthology.', default=False, action='store_true')
    parser.add_argument('-u', '--update', type=str, required=False, help='Update the ortholog tables database by adding or removing input proteomes. Performs only required alignments (if any) for new species pairs, and re-compute the ortholog groups.\nNOTE: an ID for the update must be provided.', default=None)
    parser.add_argument('-ml', '--max-len-diff', type=float, required=False, help='Maximum allowed length-difference-ratio between main orthologs and canditate inparalogs.\nExample: 0.5 means one of the two sequences could be two times longer than the other\n 0 means no length difference allowed; 1 means any length difference allowed. Default=0.5', default=0.5)
    parser.add_argument('-mgs', '--max-gene-per-sp', type=int, required=False, help='Limits the maximum number of genes per species in the multi-species output table. This option reduces the verbosity of the multi-species output file when comparing a high number of species (especially eukaryotes). Default=10', default=10)
    parser.add_argument('-sm', '--skip-multi-species', required=False, help='Skip the creation of multi-species ortholog groups.', default=False, action='store_true')
    parser.add_argument('-op', '--output-pairs', required=False, help='Output a text file with all the orthologous relations.', default=False, action='store_true')
    parser.add_argument('-qfo11', '--qfo-2011', required=False, help='Output a text file with all the orthologous relations formatted to be uploaded to the QfO benchmark service.\nNOTE: implies --output-pairs', default=False, action='store_true')
    parser.add_argument('-ka', '--keep-raw-alignments', required=False, help='Do not delete raw MMseqs2 alignment files. NOTE: this will triple the space required for storing the results.', default=False, action='store_true')
    parser.add_argument('-d', '--debug', required=False, help='Output debug information.', default=False, action='store_true')
    args = parser.parse_args()
    return (args, parser)



def check_gcc():
    '''Check that gcc is installed'''
    from sh import which
    shOut = which('gcc')
    #print('Check gcc (c++) compiler...')
    #print(shOut)
    if not shOut is None:
        from sh import gcc
        version = str(gcc('--version'))
        #print(version)
        return (version, True)
    else:
        print('ERROR: you must install gcc version 5.1 or above before continuing')
        return (None, False)



def check_quick_multiparanoid_installation(gccVer=None, gccOk=False):
    '''Check that the C++ files have been compiled.'''
    quickparaRoot = orthodetect.get_quick_multiparanoid_src_dir()

    # check for the existance of qa1
    qaPath = os.path.join(quickparaRoot, 'qa1')
    qa1ok = True
    if not os.path.isfile(qaPath):
        qa1ok = False

    # check for the existance of qa2
    qaPath = os.path.join(quickparaRoot, 'qa2')
    qa2ok = True
    if not os.path.isfile(qaPath):
        qa2ok = False

    # if one of the binaries is missing then compile
    if not (qa1ok and qa2ok):
        if not gccOk:
            sys.stderr.write('\nERROR: no gcc compiler was found in your system.\n')
            sys.stderr.write('Please, go to https://gcc.gnu.org\n')
            sys.stderr.write('And follow the instructions to install the latest version of GCC in your system.\n')
            sys.exit(-5)
        # continue and compile the binaries
        compile_quick_multiparanoid(quickparaRoot, gccVer)



def check_input_files(inputDir, debug=False):
    """Check that the input FASTA file sequence headers does not contain any blank."""
    #from collections import OrderedDict
    flist = os.listdir(inputDir)
    #substituteSymbol = '_'
    substituteSymbol = '|'
    modifiedList = []
    sys.stdout.write('\nChecking that input FASTA files does not contain blanks in headers...\n')
    for f in flist:
        if f.startswith('.DS_'):
            continue
        if '.' in f:
            sys.stderr.write('\nERROR: the file %s contains the \'.\' character in its name.\n'%f)
            sys.stderr.write('Your input file names must not contain any \'.\' in its name, please rename your proteome files accordingly.\n')
            sys.exit(-5)
        # set the path tp the input file
        path = os.path.join(inputDir, f)
        # check that the sequences are proteins
        seqtools.checkMoleculeType(path, debug)
        # now check the headers
        seqCnt, badHdr, hasBlanks, hasMultiGt = seqtools.checkFastaHdrForBlanks(path, debug)
        if hasBlanks or hasMultiGt:
            sys.stderr.write('\nWARNING: the file %s contains blanks or multiple \'>\' symbols in the FASTA headers at sequence number %d.\n'%(f, seqCnt))
            sys.stderr.write('\nFollowing is the header containing blanks and/or extra \'>\' symbols.\n')
            sys.stderr.write('%s\n'%badHdr)
            sys.stderr.write('SonicParanoid will automatically replace the blanks with and extra \'>\' symbols with \'%s\' characters.\n'%(substituteSymbol))
            sys.stderr.write('For readability of the output it is suggested that the fasta headers\ndo not contain blank symbols.\n')
            sys.stderr.write('Only the first symbol is allowed to be a \'>\' in a FASTA description line.\n')
            modifiedList.append(seqtools.formatFastaHdr(path, newSymbol=substituteSymbol, debug=debug))
        else:
            sys.stdout.write('%s:\t%d sequences\tOK!\n'%(f, seqCnt))
    #sys.exit('DEBUG')
    # return list with modified files
    return modifiedList



def check_mmseqs_installation(root):
    """Check if mmseqs has been installed."""
    #print('\n-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-')
    #print('Checking bundled mmseqs...')
    binDir = os.path.join(root, 'bin/')
    mmseqsPath = os.path.join(binDir, 'mmseqs')
    # first check if the file exists
    if not os.path.isfile(mmseqsPath):
        sys.stderr.write('\nWARNING: the MMseqs2 version supported by SonicParanoid was not found.\n')
        sys.stderr.write('Please build the version shipping with SonicParanoid following the instruction at\nhttp://iwasakilab.bs.s.u-tokyo.ac.jp/sonicparanoid\n')
        sys.exit(0)
    else:
        cmd = mmseqsPath
        process = subprocess.Popen(cmd, shell=True, stdout=subprocess.PIPE, stderr=subprocess.PIPE)
        stdout_val, stderr_val = process.communicate() # get stdout and stderr
        process.wait()
        stdout_val = stdout_val.decode()
        stderr_val = stderr_val.decode()

        # extract the supported version
        readmePath = os.path.join(binDir, 'README.txt')
        supportedVer = get_mmseqs_supported_version(readmePath)
        currentVer = None

        # if there is no error, try to extract the version
        if len(stderr_val) == 0:
            lines = stdout_val.split('\n')
            for ln in lines:
                if len(ln) == 0: # skip empty lines
                    continue
                if ln[:3]=='MMs':
                    # check if it is the version line
                    if ln[:9]=='MMseqs2 V':
                        # extract the version
                        currentVer = ln.split(': ', 1)[-1].strip()
        else:
            sys.stderr.write('\nERROR: something went wrong with the MMseqs2 installation.')
            sys.stderr.write('Please go to\nhttp://iwasakilab.bs.s.u-tokyo.ac.jp/sonicparanoid\nfor instructions on how to obtain and build the supported version of MMseqs2.')
            sys.exit(-5)

        # compare the two versions
        if currentVer is not None:
            if currentVer != supportedVer:
                sys.stderr.write('\nWARNING: the provided MMseqs2 version (%s) is different from the version supported by SonicParanoid ({:s}).\n'.format(supportedVer, currentVer))
                sys.stderr.write('Please go to\nhttp://iwasakilab.bs.s.u-tokyo.ac.jp/sonicparanoid\nfor instructions on the installation.')
                sys.exit(0)
            else: # everything went fine
                return True
        else:
            sys.stderr.write('\nWARNING: the MMseqs2 version could not be extracted. Were the binaries installed?\n')
            sys.stderr.write('Please go to\nhttp://iwasakilab.bs.s.u-tokyo.ac.jp/sonicparanoid\nfor instructions on the installation.')
            sys.exit(0)



def compile_quick_multiparanoid(srcDir, gccVer):
    '''Compile the program for multi-species orthology.'''
    sys.stderr.write('\n-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-')
    print('\nCompiling the program for multi-species orthology...')
    print('GCC:\t{:s}'.format(str(gccVer)))
    # compile inpyranoid
    from sh import make
    # move into the src directory
    prevDir = os.getcwd()
    os.chdir(srcDir)
    # first remove any previously compiled file
    shOut = make('clean')
    print('Cleaning any previous installation...')
    print(shOut)
    print('Make...')
    shOut = make('qa')
    print(shOut)
    # return to the previous directory
    os.chdir(prevDir)
    print('-#-#-#-#-#-# DONE #-#-#-#-#-#-')



def cleanup(rootDir=os.getcwd(), debug=False):
    """Remove not required files."""
    if debug:
        print('cleanup :: START')
        print('Root directory:\t%s'%rootDir)
    # list with ending 'wildcard' to detect files to be removed
    wildList = ['.c', '.cpp', '.h', '.o', '.so']
    # traverse the directory
    for dirPath, dirNames, fNames in os.walk(rootDir):
        if dirPath == rootDir:
            continue
        # remove temporary input files
        if os.path.basename(dirPath) == 'input':
            for f in fNames:
                tmpPath = '%s/%s'%(dirPath, f)
                os.remove(tmpPath)
            continue
        # check if the file name is inclued in the wildcard
        for f in fNames:
            for wcard in wildList:
                if f.endswith(wcard):
                    tmpPath = '%s/%s'%(dirPath, f)
                    os.remove(tmpPath)
            # check if it is mmseqs database file
            if fnmatch.fnmatch(f, '*.mmseqs2db*'):
                tmpPath = '%s/%s'%(dirPath, f)
                os.remove(tmpPath)



def get_mmseqs_supported_version(readmePath):
    """Read a text file and extract the Mmseqs version"""
    if not os.path.isfile(readmePath):
        sys.stderr.write('\nERROR: the file with MMseqs2 version information was not found.\n')
        sys.stderr.write('\nProvided path:\n{:s}\n'.format(readmePath))
        sys.exit(-5)
    # open and read the readme file
    fd = open(readmePath, 'r')
    # skip the first 2 lines...
    fd.readline()
    fd.readline()
    vLine = fd.readline().strip()
    fd.close()
    # return the supported version
    return vLine



########### MAIN ############
def main():

    # check that everything has been installed correctly
    root = os.path.dirname(os.path.abspath(__file__))
    root += '/'

    # get gcc version
    ##gccVer, gccOk = check_gcc()
    # compile binaries for multi-species orthology if required
    '''
    check_quick_multiparanoid_installation(gccVer, gccOk)
    check_quick_multiparanoid_installation(gccVer, gccOk)
    '''
    #### FORCE IT TO OK
    check_quick_multiparanoid_installation('FORCED TO TRUE: Sytem specific GCC version', True)
    ####
    # check MMseqs2 installation
    check_mmseqs_installation(root)

    # start measuring the execution time
    ex_start = time.perf_counter()
    #Get the parameters
    args, parser = get_params()
    #start setting the needed variables
    debug = args.debug
    inDir = None
    if args.input_directory is not None:
        inDir = '%s/'%os.path.realpath(args.input_directory)
    # output dir
    outDir = '%s/'%os.path.realpath(args.output_directory)

    # create input directory inside the output directory
    tmpInDir = os.path.join(outDir, 'input/')
    # if it already exists make sure all files in it are removed
    if os.path.isdir(tmpInDir):
        for el in os.listdir(tmpInDir):
            os.remove(os.path.join(tmpInDir, el))
    else:
        systools.makedir(tmpInDir)
    # move input files to the temporary input directory
    inProtCnt = 0
    for f in os.listdir(inDir):
        if f.startswith('.'):
            continue
        tmpPath = os.path.join(inDir, f)
        inProtCnt += 1
        if os.path.isfile(tmpPath):
            systools.copy(tmpPath, tmpInDir, debug=debug)
    # INPUT CHECK
    inDir = tmpInDir
    # check integrity of input files
    inputModList = check_input_files(inDir, debug=debug)
    # rename the input files if where required
    for tpl in inputModList:
        newPath, newSymbol = tpl
        print(newPath)
        oldPath = newPath.replace('_no_blanks', '')
        # remove the old file and rename the one with valid headers
        if os.path.isfile(oldPath):
            print(oldPath)
            os.remove(oldPath)
            systools.move(newPath, oldPath, debug=debug)
    if debug:
        print('Input files which FASTA header was modified:\t%d'%len(inputModList))
    # Optional directories setup
    alignDir = None
    if args.shared_directory is not None:
        alignDir = '%s/'%os.path.realpath(args.shared_directory)
    else:
        alignDir = os.path.join(outDir, 'alignments/')
        systools.makedir(alignDir)
    dbDirectory = None
    if args.mmseqs_dbs is not None:
        dbDirectory = '%s/'%os.path.realpath(args.mmseqs_dbs)
    else:
        dbDirectory = '%smmseqs2_databases/'%outDir
    cpus = args.threads
    #coff = args.cutoff
    coff = 40
    owOrthoTbls = args.overwrite_tables
    multiOnly = args.multi_species_only
    skipMulti = args.skip_multi_species
    runMode = args.mode
    maxGenePerSp = args.max_gene_per_sp

    # set the sensitivity value for MMseqs2
    sensitivity = 4.0 # default
    if runMode == 'sensitive':
        sensitivity = 6.0
    elif runMode == 'fast':
        sensitivity = 2.5
    elif runMode == 'most-sensitive':
        sensitivity = 7.5
    overwrite = args.overwrite
    if overwrite:
        owOrthoTbls = True

    # set sensitivity using a user spcified value if needed
    if args.sensitivity:
        if 1. <= args.sensitivity <= 7.5:
            sensitivity = round(args.sensitivity, 1)
            print('WARNING: the run mode \'%s\' will be overwritten by the custom MMseqs sensitivity value of %s.\n'%(runMode, str(args.sensitivity)))
        else:
            sys.stderr.write('\nERROR: the sensitivity parameter must have a value between 1.0 and 7.5\n')

    # set the maximum length difference allowed if difference from default
    if args.max_len_diff != 0.5:
        if not (0. <= args.max_len_diff <= 1.0):
            sys.stderr.write('\nERROR: the legth difference ratio must have a value between 0 and 1.\n')

    # set the variable to control the creation of orthologous pairs
    output_relations = args.output_pairs
    if args.qfo_2011:
        output_relations = True

    updateId = None
    if args.update is not None:
        updateId = args.update
    #check that mutually exclusive options are not selected
    if multiOnly and skipMulti:
        sys.stderr.write('\nERROR: you cannot select the options --skip-multi-species and --multi-species-only at the same time.\n')
        sys.exit(-5)
    # set the variable for MMseqs2 database indexing
    idx_dbs = True
    if args.no_indexing:
        idx_dbs = False
    # name for multispecies groups
    multiSpeciesClstrNameAll = 'multispecies_clusters.tsv'
    print('\nSonicParanoid will be executed with the following parameters:')
    print('Input directory:\t{:s}'.format(inDir))
    print('Input proteomes:\t{:d}'.format(inProtCnt))
    print('Output directory:\t{:s}'.format(outDir))
    print('Alignments directory:\t{:s}'.format(alignDir))
    print('Create pre-filter indexes:\t{:s}'.format(str(idx_dbs)))
    print('Complete overwrite:\t{:s}'.format(str(overwrite)))
    print('Re-create ortholog tables:\t{:s}'.format(str(owOrthoTbls)))
    print('CPUs:\t{:d}'.format(cpus))
    print('Run mode:\t%s (MMseqs2 s=%s)'%(runMode, str(sensitivity)))

    # check that the input directory has been provided
    if (inDir is None):
        sys.stderr.write('\nERROR: no input species.\n')
        parser.print_help()

    # Check if the run already exists
    spFile = pairsFile = None
    # SUGGEST THE USER TO USE THE UPDATE FEATURE
    spFile = os.path.join(outDir, 'species.txt')
    # check if it is an update or not
    if os.path.isfile(spFile) and (not owOrthoTbls):
        # fill a set with the new species names
        newfSet = set()
        flist = os.listdir(inDir)
        for f in flist:
            if f.startswith('.DS_'):
                continue
            newfSet.add(f)
        # now create a set from old species list
        oldfSet = set()
        for ln in open(spFile):
            oldfSet.add(ln.rstrip('\n'))
        # make the union of the two sets
        unionSet = oldfSet.union(newfSet)
        # if the length is different than suggest the user to use
        # the update or overwrite option
        if len(unionSet) != oldfSet:
            if updateId is None:
                sys.stderr.write('\n\nThe file with species already exists, but the new species list is different from the existing one.')
                sys.stderr.write('\nThis suggests that you have added, or removed species from the input directory.')
                sys.stderr.write('\nPlease consider using the \'--update update_name\' option.')
                sys.stderr.write('\nAlternatively you could completely overwrite a previous run using the \'--overwrite\' option.\n')
                sys.exit(-6)
    else:
        spFile = None

    # start the processing
    update_run = False

    if updateId is None:
        if multiOnly: #skip everything and execute directly multi-species clustering
            spFile = '%sspecies.txt'%outDir
            pairsFile = '%sspecies_pairs.txt'%outDir
        else:
            spFile, pairsFile = orthodetect.run_sonicparanoid2_multiproc(inDir, outDir=outDir, threads=cpus, sharedDir=alignDir, mmseqsDbDir=dbDirectory, create_idx=idx_dbs, sensitivity=sensitivity, cutoff=coff, confCutoff=0.05, lenDiffThr=args.max_len_diff, overwrite_all=overwrite, overwrite_tbls=owOrthoTbls, update_run=update_run, keepAlign=args.keep_raw_alignments, debug=debug)
        #run multi-species clustering
        if not skipMulti:
            #copy sqltables
            multiOutDir = os.path.join(outDir, 'multi_species/')
            sqlPaths = orthodetect.fetch_sql_files(rootDir=outDir, outDir=multiOutDir, pairsFile=pairsFile, coreOnly=False, debug=debug)
            print('Ortholog tables loaded for multi-species orthology:\t%d'%len(sqlPaths))
            sys.stdout.write('\nCreating ortholog groups...')
            multisp_start = time.perf_counter()
            quickparaRoot = orthodetect.get_quick_multiparanoid_src_dir()
            #create the multi-species clusters
            orthodetect.run_quickparanoid(sqlTblDir=multiOutDir, outDir=multiOutDir, srcDir=quickparaRoot, outName=multiSpeciesClstrNameAll, speciesFile=spFile, maxGenePerSp=maxGenePerSp, debug=debug)
            sys.stdout.write('Ortholog groups creation elapsed time (seconds):\t{:s}\n'.format(str(round(time.perf_counter() - multisp_start, 3))))

        # output directory for stats
        relDict = '%sortholog_relations/'%outDir
        # calculate stats on the generated clusters
        #orthodetect.calc_ortholog_group_stats(rootDir=outDir, outDir=relDict, outName=None, pairsFile=pairsFile, debug=debug)
        # ALL
        orthoRelName = 'ortholog_pairs.tsv'
        if args.qfo_2011:
            orthoRelName = 'ortholog_pairs_benchmark.tsv'
        # generate the relations
        if output_relations:
            orthodetect.extract_ortholog_pairs(rootDir=outDir, outDir=relDict, outName=orthoRelName, pairsFile=pairsFile, coreOnly=False, splitMode=args.qfo_2011, debug=debug)
    else: #Update the database
        update_run = True
        spFile, pairsFile = orthodetect.run_sonicparanoid2_multiproc(inDir, outDir=outDir, threads=cpus, sharedDir=alignDir, mmseqsDbDir=dbDirectory, create_idx=idx_dbs, sensitivity=sensitivity, cutoff=coff, confCutoff=0.05, lenDiffThr=args.max_len_diff, overwrite_all=overwrite, overwrite_tbls=owOrthoTbls, update_run=update_run, keepAlign=args.keep_raw_alignments, debug=debug)
        #run multi-species clustering
        if not skipMulti:
            #copy sqltables
            multiOutDir = os.path.join(outDir, '{:s}/'.format(updateId))
            sqlPaths = orthodetect.fetch_sql_files(rootDir=outDir, outDir=multiOutDir, pairsFile=pairsFile, coreOnly=False, debug=debug)
            print('Ortholog tables loaded for multi-species orthology:\t%d'%len(sqlPaths))
            sys.stdout.write('\nCreating ortholog groups...')
            multisp_start = time.perf_counter()
            quickparaRoot = orthodetect.get_quick_multiparanoid_src_dir()
            #create the multi-species clusters
            orthodetect.run_quickparanoid(sqlTblDir=multiOutDir, outDir=multiOutDir, srcDir=quickparaRoot, outName=multiSpeciesClstrNameAll, speciesFile=spFile, maxGenePerSp=maxGenePerSp, debug=debug)
            sys.stdout.write('Ortholog groups creation elapsed time (seconds):\t{:s}\n'.format(str(round(time.perf_counter() - multisp_start, 3))))
        # extract ortholog pairs
        relDict = '%sortholog_relations/'%outDir
        orthoRelName = '{:s}_relations.tsv'.format(updateId)
        if args.qfo_2011:
            orthoRelName = '{:s}_relations_benchmark.tsv'.format(updateId)
        if output_relations:
            orthodetect.extract_ortholog_pairs(rootDir=outDir, outDir=relDict, outName=orthoRelName, pairsFile=pairsFile, coreOnly=False, splitMode=args.qfo_2011, debug=debug)

    ex_end = round(time.perf_counter() - ex_start, 3)
    sys.stdout.write('\nTotal elapsed time (seconds):\t{:0.3f}\n'.format(ex_end))



    # remove not required files
    cleanup(rootDir=outDir, debug=debug)



if __name__ == "__main__":
    main()
