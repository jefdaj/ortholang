"""This module contains different utility making use of linux programs like awk, grep etc."""
import sys
import os
import subprocess

__module_name__ = 'System Tools'
__source__ = 'sys_tools.py'
__author__ = 'Salvatore Cosentino'
#__copyright__ = ''
__license__ = 'GPL'
__version__ = '1.1'
__maintainer__ = 'Cosentino Salvatore'
__email__ = 'salvo981@gmail.com'



def info():
    """This module contains different utility making use of linux programs like awk, grep etc."""
    print('MODULE NAME:\t%s'%__module_name__)
    print('SOURCE FILE NAME:\t%s'%__source__)
    print('MODULE VERSION:\t%s'%__version__)
    print('LICENSE:\t%s'%__license__)
    print('AUTHOR:\t%s'%__author__)
    print('EMAIL:\t%s'%__email__)



def bzip2(path, outDir=os.getcwd(), level=9, overwrite=False, keep=True, debug=False):
    """Use bzip2 to compress the input archive to the output directory."""
    import bz2
    if debug:
        print('Input file:\t%s'%path)
        print('Output dir:\t%s'%outDir)
        print('Compression level:\t%d'%level)
        print('Overwrite existing compressed file:\t%s'%overwrite)
        print('Keep original file:\t%s'%keep)
    #check if the zip file is valid
    if not os.path.isfile(path):
        sys.stderr.write('\nERROR: %s is not a valid file.'%path)
        sys.exit(-2)
    # create the output directory
    makedir(outDir)
    # create the new path
    newName = '%s.bz2'%os.path.basename(path)
    outPath = os.path.join(outDir, newName)
    # check that the output file does not already exist
    if os.path.isfile(outPath):
        if not overwrite:
            sys.stderr.write('\nERROR: the archive %s already exists.\nSet overwrite=True to overwrite.'%outPath)
            sys.exit(-2)
    # open output file
    # note that this would completely load the input file in memory
    new_file = bz2.BZ2File(outPath, mode='wb', compresslevel=level)
    # now write the data
    new_file.write(bz2.compress(open(path, 'rb').read(), compresslevel=9))
    if debug:
        print('\nThe archive\n%s\nwas compressed to\n%s'%(path, outPath))
    # keep/remove original
    if not keep:
        os.remove(path)
        if debug:
            print('\nThe original raw file \n%s\n has been removed.'%(path))
    #return the path to the extracted archive
    return outPath


'''
def bunzip2_old(path, outDir=os.getcwd(), keep=True, debug=False):
    """Use bzip2 to incrementally decompress the input archive to the output directory."""
    import bz2
    if debug:
        print('bunzip2 :: START')
        print('bz2 archive path:\t%s'%path)
        print('Output directory:\t%s'%outDir)
        print('Keep original archive:\t%s'%keep)
    #check if the bz2 file is valid
    if not os.path.isfile(path):
        sys.stderr.write('\nERROR: %s is not a valid file.'%path)
        sys.exit(-2)
    # create the path to the file uncrompressed file
    outPath = os.path.join(outDir, os.path.basename(path).replace('.bz2', ''))

    #bz2File = bz2.BZ2File(path)
    #bz2File = open(path, 'rb')
    bz2File = bz2.BZ2File(outPath)

    new_file = open(outPath, 'wb')

    #new_file.write(bz2.decompress(src_file))
    new_file.write(bz2.decompress(bz2File.read()))
    new_file.close()

    if debug:
        print('\nThe archive\n%s\nwas succesfully extracted to\n%s'%(path, outPath))
    # keep/remove original
    if not keep:
        os.remove(path)
        if debug:
            print('\nThe original archive \n%s\n has been removed.'%(path))
    return outPath
'''



def checkFormat(inFile, flag=None, debug=False):
    """
    This function will use libmagic to identify the file format.
    flag: can be eaither MAGIC_MIME_TYPE (Default), MAGIC_NONE or MAGIC_MIME_ENCODING
     and will define how the output looks
    """
    if debug:
        print('\ncheckFormat :: START')
        print('INPUT:\t%s'%inFile)
        print('FLAG:\t%s'%flag)
    #check the existence of the input file
    if not os.path.isfile(inFile):
        sys.stderr.write('The file %s was not found, please provide a input path'%inFile)
        sys.exit(-2)
    flagList = ['MAGIC_MIME_TYPE', 'MAGIC_NONE', 'MAGIC_MIME_ENCODING']
    import magic
    #set the flag
    mflag = magic.MAGIC_MIME_TYPE
    if flag != None:
        if not flag.strip().upper() in flagList:
            sys.stderr('ERROR: the flag %s is not supported, please use one of the following:\n%s'%(flag, ', '.join(flagList)))
        if flag == 'MAGIC_NONE':
            mflag = magic.MAGIC_NONE
        elif flag == 'MAGIC_MIME_ENCODING':
            mflag = magic.MAGIC_MIME_ENCODING
    #let's now check the format
    myMagic = magic.Magic(flags=mflag)
    finalFormat = myMagic.id_filename(inFile)
    myMagic.close()
    if debug:
        print('FORMAT:\t%s'%finalFormat)
    #let's return the file format
    return finalFormat



def copy(src, dst, metaData=False, debug=False):
    """Copy src file/dir to dst."""
    if debug:
        print('copy :: START')
        print('SRC:\t%s'%src)
        print('DEST:\t%s'%dst)
        print('METADATA:\t%s'%metaData)
    #check the existence of the input file
    if not os.path.isfile(src):
        sys.stderr.write('The file %s was not found, please provide a valid file path'%src)
        sys.exit(-2)
    #if src and dst are same, do nothing...
    if src == dst:
        sys.stderr.write('\nWARNING: Source and destination files are the same, nothing will be done.\n')
        return False
    import shutil
    #let's execute commands
    if metaData: #then also copy the metadata
        try:
            shutil.copy2(src, dst)
        # eg. src and dest are the same file
        except shutil.Error as e:
            print('Error: %s' % e)
        # eg. source or destination doesn't exist
        except IOError as e:
            print('Error: %s' % e.strerror)
    else:
        try:
            shutil.copy(src, dst)
        # eg. src and dest are the same file
        except shutil.Error as e:
            print('Error: %s' % e)
        # eg. source or destination doesn't exist
        except IOError as e:
            print('Error: %s' % e.strerror)
    return True



def countLinesWc(inFile, debug=False):
    """Takes in input a text file and uses WC to count the number of lines."""
    if debug:
        print('countLinesWc :: START')
        print('INPUT:\n%s'%inFile)
    #check the existence of the input file
    if not os.path.isfile(inFile):
        sys.stderr.write('The file %s was not found, please provide a input path'%inFile)
        sys.exit(-2)
    #let's prepare and execute the command
    cmd = 'wc -l %s'%inFile
    tmp = subprocess.check_output(cmd, stderr=subprocess.PIPE, shell=True)
    inLines = int(tmp.split()[0])
    if debug:
        print('COUNT LINES CMD:\n%s'%cmd)
        print('COUNTED LINES:\t%d'%inLines)
    return inLines



def chopString(s, n, debug=False):
    """
    Chop strings to a given size.
    Produce (yield) \'n\'-character chunks from \'s\'.
    """
    if debug:
        print('chopString :: START')
        print('INPUT ::\t%s'%s)
        print('CHUNK LENGTH ::\t%d'%n)
        print('INPUT LENGTH ::\t%d'%len(s))
    for start in range(0, len(s), n):
        yield s[start:start+n]



def diff(f1, f2, outDir=os.getcwd(), outName=None, debug=True):
    """A wrapper for the unix diff program."""
    if debug:
        print('diff :: START')
        print('File 1:\n%s'%f1)
        print('File 2:\n%s'%f2)
        print('Output dir:\t%s'%outDir)
    #check the existence of the input files
    if not os.path.isfile(f1):
        sys.stderr.write('The file %s was not found, please provide a input path'%f1)
        sys.exit(-2)
    if not os.path.isfile(f2):
        sys.stderr.write('The file %s was not found, please provide a input path'%f2)
        sys.exit(-2)
    #create the output directory if does not exist yet
    if outDir[-1] != '/':
        outDir += '/'
    #create output directory
    makedir(outDir)
    #set the output name
    outputName = ''
    if outName is not None:
        if type(outName) is str:
            outputName = outName.strip()
            #outputName = ''.join(outputName.split(' ')) #remove intenal spaces if any
            outputName = outputName.replace(' ', '') #remove intenal spaces if any
    else:
        outName = '%s-%s_diff.txt'%(os.path.basename(f1), os.path.basename(f2))
    #output file
    outPath = '%s%s'%(outDir, outName)
    #Unix diff example
    # diff <f1> <f2>
    cmd = 'diff %s %s > %s'%(f1, f2, outPath)
    process = subprocess.Popen(cmd, shell=True, stdout=subprocess.PIPE, stderr=subprocess.PIPE)
    stdout_val, stderr_val = process.communicate() #get stdout and stderr
    process.wait()
    if debug:
        print('Diff command:\n%s'%str(cmd))
    if debug:
        print('Diff STDOUT:\n%s'%stdout_val)
        print('Diff STDERR:\n%s'%stderr_val)
    #check the length of the diff file
    diffLen = countLinesWc(outPath, debug=debug)
    if diffLen == 0:
        return (False, outPath)
    else:
        return (True, outPath)



def evalCpuNeeds(totLn, algorithm=None, debug=False):
    """
    Estimates the number of needed threads (cores) based on the total lines to be processed
    For the supported algorithms an estimation is given but should be updated when possible
    NOTES: it is very experimental and based on a very few samples
    """
    import multiprocessing
    maxCores = multiprocessing.cpu_count()
    buckets = [2, 4, 8, 12, 16, 24, 32, 48, 64, 96, 128]
    #input summary...
    if debug:
        print('\n evalCpuNeeds START:')
        print('TOT LINES ::\t%s'%str(totLn))
        print('ALGORITHM ::\t%s'%str(algorithm))
    chunkDict = {'bwa':720000, 'map_pct_calc':400000, 'avg':560000}
    #if all the arguments are NONE then we set bwa as algorithm
    if algorithm == None:
        algorithm = 'avg'
    chunkSize = chunkDict[algorithm]
    if chunkSize >= totLn:
        if debug:
            print('Chunk size is bigger than the line to be porcessed, 1 core will do just fine...')
        return 1
    cores = int(totLn/chunkSize)
    if debug:
        print('Selected Algorimth for Estimation:\t%s'%algorithm)
        print('CHUNK SIZE FOR %s ALGORITHM:\t%d'%(algorithm.upper(), chunkSize))
    if cores >= maxCores:
        return maxCores
    #selelct the bucket if needed
    if algorithm != 'map_pct_calc':
        for el in buckets:
            if float(cores/el) <= 1:
                return el
    return cores



def getCpuCount():
    """Get the number of cpu available in the system."""
    import multiprocessing
    return multiprocessing.cpu_count()



def getElapsedTime(f1, f2, timeStamps=False, debug=True):
    """Calculate the elapsed time between the creation of the first file and last access to the second."""
    if debug:
        print('getElapsedTime :: START')
        print('File 1:\n%s'%f1)
        print('File 2:\n%s'%f2)
        print('Timestamps:\n%s'%timeStamps)
        #if timeStamps is True then each file must contain a single unix timestamp
    #check the existence of the input files
    if not os.path.isfile(f1):
        sys.stderr.write('The file %s was not found, please provide a input path'%f1)
        sys.exit(-2)
    if not os.path.isfile(f2):
        sys.stderr.write('The file %s was not found, please provide a input path'%f2)
        sys.exit(-2)
    bf1 = os.path.basename(f1)
    bf2 = os.path.basename(f2)
    #Do in a different way depending if the files contain timestamps or not
    ts1_ct = ts2_mt = None
    if timeStamps:
        tmpFd = open(f1)
        ts1_ct = int(tmpFd.readline().strip()) #read the timestamp
        tmpFd.close()
        tmpFd = open(f2)
        ts2_mt = int(tmpFd.readline().strip()) #read the timestamp
        tmpFd.close()
    else: #then use os.stat
        import stat
        ts1_ct = os.stat(f1).st_ctime #get creation time
        #NOTE: if the file has been not accessed and has been copied with its original metadata, the time could be older than then ts1_ct
        ts2_mt = os.stat(f2).st_mtime #get latest access time
    #now convert them to using datetime
    import datetime as dt
    #t1 = dt.datetime.utcfromtimestamp(ts1_ct)
    #t2 = dt.datetime.utcfromtimestamp(ts2_mt)
    t1 = dt.datetime.fromtimestamp(ts1_ct)
    t2 = dt.datetime.fromtimestamp(ts2_mt)

    if debug:
        print('%s was created on %s'%(bf1, str(t1)))
        print('%s was last modified on %s'%(bf2, str(t2)))
    #no calculate the time difference
    delta = t2 - t1
    #following is the elapsed time between the creation of f1 and the last modification of f2
    sec = int(delta.total_seconds())
    minutes = round(sec/60., 2)
    hours = round(sec/3600., 2)
    days = round(sec/86400.656, 2)
    if debug:
        print('Elapsed time (%s - %s)'%(bf2, bf1))
        print('Seconds:\t%d'%sec)
        print('Minutes:\t%s'%str(minutes))
        print('Hours:\t%s'%str(hours))
        print('Days:\t%s'%str(days))
    return(sec, minutes, hours, days)



def getObjSize(obj, debug=False):
    """Calculate the size of an object (list, dictionary or tuple)."""
    from collections import OrderedDict
    supportedTypes = ['list', 'collections.OrderedDict', 'dict']

    if debug:
        print('getObjSize :: START')
        print('Object:\n%s'%obj)
        print('File 2:\n%s'%f2)
        print('Timestamps:\n%s'%timeStamps)

    objType = type(obj)
    size = sys.getsizeof(obj)
    if (objType is dict) or (objType is OrderedDict):
        for k in obj:
            size += sys.getsizeof(k)
            size += sys.getsizeof(obj[k])
    elif objType is list:
        for el in obj:
            size += sys.getsizeof(el)
    else:
        return -1
    return size



def getShell():
    """Get the shell type used by the system."""
    shellPath = os.environ["SHELL"]
    #check the shell type and return it
    if shellPath.endswith('csh'):
        return 'csh'
    elif shellPath.endswith('bash'):
        return 'bash'
    else: #add other shell types
        return None



def html2text(inHtml, outDir=os.getcwd(), outName=None, ignoreLinks=False, ignoreEmphasis=False, protectLinks=False, ignoreImages=False, imagesToAlt=False, bodyWidth=None, escapeAll=False, singleLineBreak=False, debug=False):
    """
    Takes as input a valid url or an html file and will prodice a text file from the input html
    file, using html2text package
    NOTE: to see all the available options for the command line version type html2text --help
    """
    #input summary...
    if debug:
        print('html2text START:')
        print('INPUT ::\t%s'%str(inHtml))
        print('OUTPUT DIR ::\t%s'%str(outDir))
        print('OUTNAME ::\t%s'%str(outName))
        print('--ignore-links ::\t%s'%str(ignoreLinks))
        print('--ignore-emphasis ::\t%s'%str(ignoreEmphasis))
        print('--protect-links ::\t%s'%str(protectLinks))
        print('--ignore-images ::\t%s'%str(ignoreImages))
        print('--images-to-alt ::\t%s'%str(imagesToAlt))
        print('--escape-all ::\t%s'%str(escapeAll))
        print('--single-line-break ::\t%s'%str(singleLineBreak))
        print('body-width ::\t%s'%str(bodyWidth))
    #import html2text as h2t #import the module that will do the dumping
    #check that the input type is either a file or a valid url
    isFile = False #used to understand if the input is a file or an url
    if os.path.isfile(inHtml):
        isFile = True
    else: #check that the url starts with http or https
        if not ((inHtml.startswith('http://')) or (inHtml.startswith('https://'))):
            sys.stderr.write('\nERROR: the url must start with either \'https://\' or \'http://\'\n')
            sys.exit(-4)
    #set the output path
    outPath = None
    if outName != None:
        outPath = '%s%s'%(outDir, outName)
    else:
        outPath = '%s%s'%(outDir, 'text2html_output.txt')
    if debug:
        print('OUTPUT ::\t%s'%str(outPath))
    #lets prepare the command to be executed
    src = inHtml
    if not isFile: #then we need to treat it as na url
        src = '\'%s\''%src
    #lets now add the options
    options = ''
    if ignoreLinks:
        options = '%s --ignore-links'%options
    if ignoreEmphasis:
        options = '%s --ignore-emphasis'%options
    if protectLinks:
        options = '%s --protect-links'%options
    if ignoreImages:
        options = '%s --ignore-images'%options
    if imagesToAlt:
        options = '%s --images-to-alt'%options
    if escapeAll:
        options = '%s --escape-all'%options
    if singleLineBreak:
        options = '%s --single-line-break'%options
        bodyWidth = 0#this is requested by html2text program
    if bodyWidth != None:
        if bodyWidth < 0:
            bodyWidth = 0
        options = '%s --body-width=%d'%(options, bodyWidth)
    #EXAMPLE: html2text <input file | url> [options]
    h2tCmd = 'html2text %s %s'%(options, src)
    #execute the command
    process = subprocess.Popen(h2tCmd, shell=True, stdout=subprocess.PIPE, stderr=subprocess.PIPE)
    stdout_val, stderr_val = process.communicate() #get stdout and stderr
    process.wait()
    if debug:
        print('HTML2TEXT CMD:\n%s'%str(h2tCmd))
    #open output file and write it
    fd = open(outPath, 'w')
    fd.write(stdout_val)
    if stderr_val != '':
        fd.write(stderr_val)
    fd.close()
    #return the output
    return outPath



def makedir(path):
    """Create a directory including the intermediate directories in the path if not existing."""
    try:
        os.makedirs(path)
    except OSError:
        if not os.path.isdir(path):
            raise



def move(src, dst, debug=False):
    """Recursively moves src to dst."""
    if debug:
        print('move :: START')
        print('SRC:\n%s'%src)
        print('DEST:\n%s'%dst)
    #check the existence of the input file
    if not os.path.exists(src):
        sys.stderr.write('%s was not found, please provide a valid path'%src)
        sys.exit(-2)
    import shutil
    #let's execute command
    if os.path.exists(dst): #then we should use copy
        #copy and remove the source file
        copy(src, dst, True, debug)
        os.remove(src)
    else:
        shutil.move(src, dst)



def splitTxtFileAwk(inFile, outDir=os.getcwd(), inLines=None, chunks=2, suffix=None, debug=False):
    """Takes in input a text file and uses AWK to split the in n (chunks) part of almost the same size."""
    if debug:
        print('splitTxtFileAwk :: START')
        print('INPUT:\n%s'%inFile)
        print('OUT DIR ::\t%s'%outDir)
        print('INPUT LINES ::\t%s'%str(inLines))
        print('CHUNKS ::\t%d'%chunks)
        print('OUTPUT SUFFIX ::\t%s'%str(suffix))
    #check the existence of the input file
    if not os.path.isfile(inFile):
        sys.stderr.write('The file %s was not found, please provide a input path'%inFile)
        sys.exit(-2)
    if not os.path.isdir(outDir):
        sys.stderr.write('ERROR: the output directory %s does not exist'%outDir)
        sys.exit(-2)
    #check that the number of chunk is at least 2
    if chunks < 2:
        sys.stderr.write('WARNING: the chunks parameter must be at least 2 nothing will be done for the input file')
        sys.exit(-3)
    #count the number of lines if needed
    if inLines == None:
        cmd = 'wc -l %s'%inFile
        tmp = subprocess.check_output(cmd, stderr=subprocess.PIPE, shell=True)
        inLines = int(tmp.split()[0])
        if debug:
            print('COUNT LINES CMD:\n%s'%cmd)
            print('COUNTED LINES:\t%d'%inLines)
    if suffix == None:
        suffix = '_part'
    outPartName = os.path.basename(inFile)
    flds = outPartName.split('.')
    outPartName = flds[0]
    del flds
    outPartName = outPartName + suffix
    #estimate the size of each chunk
    chunkSize = 0
    while True:
        #if debug:
            #print('%d * %d = %d'%(chunkSize, chunks, chunkSize*chunks))
        if chunkSize*chunks < inLines:
            chunkSize += 1
        else:
            break
    if debug:
        print('CHUNK SIZE:%d'%chunkSize)
    #AWK SPLIT EXAMPLE
    #awk 'NR%3000==1{x="sam_chunk_"++i;}{print > x}' stool_ion_nsf001_se_mapped_bwa_mapped.sam
    splitCmd = 'awk \'NR%%%d==1{x="%s"++i;}{print > x}\' %s'%(chunkSize, outDir + outPartName, inFile)
    if debug:
        print('AWK SPLIT CMD:\n%s'%splitCmd)
    #execute the system call
    process = subprocess.Popen(splitCmd, shell=True, stdout=subprocess.PIPE, stderr=subprocess.PIPE)
    stdout_val, stderr_val = process.communicate() #get stdout and stderr
    process.wait()
    if debug:
        print('\nSTDOUT:\n%s'%repr(stdout_val))
        print('\nSTDERR:\n%s'%repr(stderr_val))
    #check that all the parts have been created
    partsList = []
    for i in range(1, chunks+1):
        path = outDir + outPartName + str(i)
        if os.path.isfile(path):
            partsList.append(path)
        else:
            sys.stderr.write('ERROR: not all the file parts were successfully created...')
            if debug:
                print(path)
            sys.exit(-1)
    if debug:
        print('created files:\t%d'%len(partsList))
        #print(str(partsList))
    return partsList



def untar(tarpath, outDir=os.getcwd(), debug=True):
    import tarfile
    if debug:
        print('Tar.gz path:\t%s'%tarpath)
        print('Output dir:\t%s'%outDir)
    #check if the tar file is valid
    if not tarfile.is_tarfile(tarpath):
        sys.stderr.write('\nERROR: %s is not a valid tar.gz file.'%tarpath)
        sys.exit(-2)
    #create the output directory if does not exist yet
    if outDir[-1] != '/':
        outDir += '/'
    #create output directory
    makedir(outDir)
    #change current directory
    cwd = os.getcwd()
    if os.path.dirname(outDir) != cwd:
        os.chdir(outDir)
    #open the tar file
    tar = tarfile.open(tarpath)
    tar.extractall()
    tar.close()
    #set the current directory to the starting one
    os.chdir(cwd)
    if debug:
        print('Extracted in %s'%outDir)



def unzip(path, outDir=os.getcwd(), debug=False):
    """Unzip the input archive to the output directory."""
    import zipfile
    if debug:
        print('Zip path:\t%s'%path)
        print('Output dir:\t%s'%outDir)
    #check if the zip file is valid
    if not zipfile.is_zipfile(path):
        sys.stderr.write('\nERROR: %s is not a valid zip file.'%path)
        sys.exit(-2)
    # Create a ZipFile Object Instance
    archive = zipfile.ZipFile(path, 'r')
    zipInfo = archive.infolist()
    #get the root directory name
    zipRootName = zipInfo[0].filename
    cwd = os.getcwd()
    if os.path.dirname(outDir) != cwd:
        os.chdir(outDir)
    unarchivedPath = '%s%s'%(outDir, zipRootName)
    if os.path.isdir(unarchivedPath):
        sys.stderr.write('\nWARNING: the directory %s already exists! Its content will be overwritten.'%unarchivedPath)
    #list files in the archive
    archive.extractall(outDir)
    archive.close()
    if not os.path.isdir(unarchivedPath):
        sys.stderr.write('\nERROR: the archived extraction went wrong.')
        sys.stderr.write('\nThe uncompressed directory %s does not exist.'%unarchivedPath)
        sys.exit(-3)
    #set the current directory to the starting one
    os.chdir(cwd)
    if debug:
        print('\nThe archive\n%s\nwas succesfully extracted to\n%s'%(path, unarchivedPath))
    #return the path to the extracted archive
    return unarchivedPath



def test_bunzip2(debug=False):
    """Test the function to bunzip2 an archive."""
    archiveDir = '/home/salvocos/tmp/test_sys_tools/input/'
    inBz = '%smmseqs_qfo_S40_avx2_core_relations.tsv.bz2'%(archiveDir)
    outDir = '/home/salvocos/tmp/'
    keep = False
    #unarchive
    bunzip2(inBz, outDir=outDir, keep=keep, debug=debug)



def test_bzip2(debug=False):
    """Test the function to bzip2 an archive."""
    archiveDir = '/home/salvocos/tmp/test_sys_tools/input/'
    inRaw = '%smmseqs_qfo_S40_avx2_core_relations.tsv'%(archiveDir)
    outDir = '/home/salvocos/tmp/'
    overwrite = True
    keep = False
    lev = 9
    #unarchive
    bzip2(inRaw, outDir=outDir, level=lev, overwrite=overwrite, keep=keep, debug=debug)



def test_checkFormat(debug=True):
    """Test check file format."""
    inFile = '/user/gen-info/salvocos/projects/pathogenFinder2/models/ecoli/models/test_dir/make_ecoli_model_test_n2c04t64m64d0g1_org2_04_10_fam.fa.gz'
    #flagList = ['MAGIC_MIME_TYPE', 'MAGIC_NONE', 'MAGIC_MIME_ENCODING']
    #default flag
    flag = None
    fmt = checkFormat(inFile, flag, debug)
    if debug:
        print(fmt)
    #zip
    inFile = '/user/gen-info/salvocos/projects/pathogenFinder2/models/ecoli/models/test_dir/make_ecoli_model_test_n2c04t64m64d0g1_org2_04_10_fam.fa.zip'
    #default flag
    flag = None
    fmt = checkFormat(inFile, flag, debug)
    if debug:
        print(fmt)
    #RAR format
    inFile = '/user/gen-info/salvocos/projects/pathogenFinder2/models/ecoli/models/test_dir/make_ecoli_model_test_n2c04t64m64d0g1_org2_04_10_fam.fa.rar'
    #default flag
    flag = None
    fmt = checkFormat(inFile, flag, debug)
    if debug:
        print(fmt)
    #7Z format
    inFile = '/user/gen-info/salvocos/projects/pathogenFinder2/models/ecoli/models/test_dir/make_ecoli_model_test_n2c04t64m64d0g1_org2_04_10_fam.fa.7z'
    #default flag
    flag = None
    fmt = checkFormat(inFile, flag, debug)
    if debug:
        print(fmt)
    #TAR.GZ format
    inFile = '/user/gen-info/salvocos/projects/pathogenFinder2/models/ecoli/models/test_dir/make_ecoli_model_test_n2c04t64m64d0g1_org2_04_10_fam.fa.tar.gz'
    #default flag
    flag = None
    fmt = checkFormat(inFile, flag, debug)
    if debug:
        print(fmt)



def test_chopString(debug=True):
    """Chop strings to a given size."""
    inStr = 'abcderfghailmnopurst'
    chunksGen = chopString(inStr, 5, debug)
    for el in chunksGen:
        print(el)



def test_copy(debug=True):
    """Test the copy of files."""
    src = '/user/gen-info/salvocos/projects/pathogenFinder2/gold_data/test_gbk_conversion/209915368.gbk'
    outTestDir = '/user/gen-info/salvocos/projects/pathogenFinder2/gold_data/test_gbk_conversion/fasta/'
    #to dir
    #copy(src, outTestDir, False, debug)
    #to dir (metadata)
    #copy(src, outTestDir, True, debug)
    #to complete target (metadata)
    #copy(src, outTestDir+'minchia_meta.gbk', True, debug)
    #to complete target
    copy(src, outTestDir+os.path.basename(src), False, debug)



def test_countLinesWc(debug=False):
    """Test the function to count lines."""
    #input
    inFile = '/user/gen-info/salvocos/test_directory/samtools/stool_ion_nsf001_se_mapped_bwa.sam'
    #no input lines
    countLinesWc(inFile, debug)



def test_diff(debug=False):
    """Test the function to execute the UNIX diff."""
    #DEFINTION: diff(f1, f2, outDir=os.getcwd(), outName=None, debug=True):
    root = '/home/salvocos/tmp/test_sys_tools/'
    f1 = '%sinput/a.txt'%(root)
    f2 = '%sinput/b.txt'%(root)
    f3 = '%sinput/c.txt'%(root)
    outDir = '%sdiff/'%root
    #f1 != f2
    differ, diffPath = diff(f1, f2, outDir=outDir, outName=None, debug=debug)
    if debug:
        print('The input file are different:\t%s'%differ)
        print('Diff output file path:\t%s'%diffPath)
    #f2 == f3
    differ, diffPath = diff(f2, f3, outDir=outDir, outName='f2-f3.diff.txt', debug=debug)
    if debug:
        print('The input file are different:\t%s'%differ)
        print('Diff output file path:\t%s'%diffPath)



def test_evalCpuNeeds(debug=False):
    """Test the function to evaluate the number of needed cpus."""
    #EXAMPLE
    #evalCpuNeeds(totLn, algorithm=None, debug=False)
    #50M reads with BWA
    cores = evalCpuNeeds(50000000, 'bwa', debug)
    print('SUGGESTED NUMBER OF THREADS:\t%d'%cores)
    #5M reads, no algorithm
    cores = evalCpuNeeds(5000000, None, debug)
    print('SUGGESTED NUMBER OF THREADS:\t%d'%cores)



def test_getElapsedTime(debug=True):
    """Test estimation of elpsed time between creation and modification of 2 files."""
    #DEFINTION: getElapsedTime(f1, f2, debug=True)
    inputDir = '/home/salvocos/projects/fungal_genomes_riken/data/inparanoid_runs/ortholog_search/jcm_3685-jcm_9195/'
    f1 = '%sBLOSUM80'%(inputDir)
    #f2 = '%sOutput.jcm_3601-jcm_11330'%(inputDir)
    logFile = '%sinparanoid.log'%(inputDir)
    getElapsedTime(f1, logFile, timeStamps=False, debug=debug)
    #latest blastrun vs inparalog.log -> run_inparalog time
    lastBlast = '%sjcm_9195-jcm_3685'%(inputDir)
    getElapsedTime(lastBlast, logFile, timeStamps=False, debug=debug)
    #Use timestamps
    root = '/home/salvocos/tmp/test_sys_tools/'
    f1 = '%sinput/start_blast_normal'%(root)
    f2 = '%sinput/end_blast_normal_aa'%(root)
    getElapsedTime(f1, f2, timeStamps=True, debug=debug)



def test_getShell(debug=True):
    """test the function that returns the system shell type"""
    print(getShell())



def test_html2text(debug=True):
    """Test the function to convert html to text."""
    outDir = '/user/gen-info/salvocos/tmp/html_dump/output/'
    url = 'https://gold.jgi-psf.org/projects?id=61'
    inHtml = '/user/gen-info/salvocos/tmp/html_dump/input_html/html2text_PythonPackage.html'
    #html file as input
    #ignoreLinks=False, ignoreEmphasis=False, protectLinks=False, ignoreImages=False, imagesToAlt=False, bodyWidth=None, escapeAll=False, singleLineBreak=False, debug=False):
    outFile = html2text(inHtml, outDir, 'html2text_page_html_from_file.txt', ignoreImages=True, debug=True)
    #url input
    outFile = html2text(url, outDir, 'gold_page_from_url.txt', ignoreEmphasis=True, ignoreImages=True, escapeAll=True, singleLineBreak=True, debug=True)
    if debug:
        print(outFile)



def test_splitTxtFileAwk(debug=False):
    """Test the function split a file."""
    #EXAMPLE: splitTxtFileAwk(inFile, outDir=os.getcwd(), inLines=None, chunks=2, suffix= '_part', debug=False)
    inFile = '/user/gen-info/salvocos/test_directory/samtools/stool_ion_nsf001_se_mapped_bwa.sam'
    outDir = '/user/gen-info/salvocos/test_directory/samtools/tmp_stats/'
    #no input lines
    splitTxtFileAwk(inFile, outDir, None, 64, None, debug)



def test_unzip(debug=False):
    """Test the function to unzip an archive."""
    #DEFINTION: unzip(path, outDir=os.getcwd(), debug=False):
    archivesDir = '/home/salvocos/projects/fungal_genomes_riken/data/original_from_jcm/archives/'
    inZip = '%sJCM_9478.zip'%(archivesDir)
    outDir = '/home/salvocos/tmp/'
    #unarchive
    unzip(inZip, outDir=outDir, debug=debug)



def test_untar(debug=False):
    """Test the function to untar an archive."""
    #DEFINTION: untar(path, outDir=os.getcwd(), debug=True)
    root = '/home/salvocos/tmp/test_sys_tools/'
    inTargz = '%sinput/inparanoid_salvo_mod.tar.gz'%(root)
    outDir = root + 'input/'
    #unarchive
    untar(inTargz, outDir=outDir, debug=debug)
