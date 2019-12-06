'''
This module is used to execute blast and related tools
it also include some functions to format blast output
'''
import sys
import os
from collections import OrderedDict
import subprocess
import sys_tools as systools

__module_name__ = "Blast Tools"
__source__ = "blast_tools.py"
__author__ = "Salvatore Cosentino"
#__copyright__ = ""
__license__ = "GPL"
__version__ = "1.3"
__maintainer__ = "Cosentino Salvatore"
__email__ = "salvo981@gmail.com"


#root path for files
pySrcDir = os.path.dirname(os.path.abspath(__file__))
pySrcDir += '/'
root, d1, d2 = pySrcDir.rsplit('/', 2)
root += '/'
#root = '%s/'%os.path.dirname(os.getcwd())
bin_root = '%sbin/'%root
blastn_dbs_root = '%sdbs/blastn/'%root
blastp_dbs_root = '%sdbs/blastp/'%root
resource_root = '%sresources/'%root

#BALST PROGRAMS
blastTools = {'blastn':'%sblastn'%bin_root}
blastTools['blastp'] = '%sblastp'%bin_root
blastTools['blastx'] = '%sblastx'%bin_root
blastTools['blast_formatter'] = '%sblast_formatter'%bin_root
blastTools['makeblastdb'] = '%smakeblastdb'%bin_root

#paths for blast databases
blastDbPaths = {'ncbi_viruses_ntv':'%sntv_20150615'%blastn_dbs_root}


#blastDbPaths = {'ncbi_nt':'/imetgpfs/db/ncbi_blast/nt/20140818/nt'}
#blastDbPaths['imet_virus_nt'] = '/user/gen-info/salvocos/projects/dbs/local/blast/imetdbs/20140818/iMetDBntv_nucl_blastdb'
#blastDbPaths['imet_bacteria_nt'] = '/user/gen-info/salvocos/projects/dbs/local/blast/imetdbs/20140818/iMetDBntb_nucl_blastdb'
#blastDbPaths['ncbi_bacteria_complete_nt'] = '/user/gen-info/salvocos/projects/dbs/local/blast/ncbi_bacteria/230315/nucl/ncbi_bacteria_nucl'
#blastDbPaths['ncbi_nt_bacteria'] = '/user/gen-info/salvocos/projects/dbs/local/blast/imetdbs/20150316/ncbi_nt_bacteria'
#blastDbPaths['ncbi_nr_bacteria'] = '/user/gen-info/salvocos/projects/dbs/local/blast/imetdbs/20150316/ncbi_nr_bacteria'
#blastDbPaths['ncbi_viruses_nucl'] = '/user/gen-info/salvocos/projects/dbs/local/blast/ncbi_viruses/230315/nucl/ncbi_viruses_nucl'
#blastDbPaths['ncbi_viruses_ntv'] = '/user/gen-info/salvocos/projects/dbs/local/blast/ncbi_viruses/20150615/ntv_20150615'

#virus gi to lineage table
#givir2lineage = '/user/gen-info/salvocos/projects/metapatfinder/resources/virus_gi2lineage.txt'


def info():
    """This module is used to execute blast and related tools it also include some functions to format blast output."""
    print('MODULE NAME:\t%s'%__module_name__)
    print('SOURCE FILE NAME:\t%s'%__source__)
    print('MODULE VERSION:\t%s'%__version__)
    print('LICENSE:\t%s'%__license__)
    print('AUTHOR:\t%s'%__author__)
    print('EMAIL:\t%s'%__email__)



def blastn(querySeq, dbPath, outDir=os.getcwd(), outName=None, eVal=0.001, outFormat='0', threads=4, debug=False):
    '''This function is used to execute blastn on query dna sequence.'''
    #from Bio.Blast.Applications import NcbiblastnCommandline
    #check that the input file and the database exist
    if not os.path.isfile(querySeq):
        sys.stderr.write('The file %s was not found, please provide the path to a valid FASTA file'%querySeq)
        sys.exit(-2)
    if (not os.path.isfile(dbPath + '.nal')) and (not os.path.isfile(dbPath + '.nsq')):
        sys.stderr.write('The database %s was not found, please create it using makeblastdb with extensions .nal and/or .nsq'%dbPath)
        sys.exit(-2)
    #print some debug
    if debug:
        print('Query:\t%s'%querySeq)
        print('Database:\t%s'%dbPath)
        print('Output dir:\t%s'%outDir)
        print('Output name:\t%s'%outName)
        print('Blastn evalue threshold:\t%s'%str(eVal))
        print('Output format:\t%s'%outFormat)
        print('Cores:\t%s'%str(threads))
    #set the output format
    blastFormat = 0
    if (outFormat == 'xml') or (outFormat == '5'):
        blastFormat = 5
        outFormat = 'xml'
    elif (outFormat == None) or (outFormat == '0'):
        blastFormat = 0
        outFormat = 'out'
    elif (outFormat == 'txt') or (outFormat == '7'):
        blastFormat = 7
        outFormat = 'out'
    elif outFormat == '11':
        blastFormat = '11'
        outFormat = 'asn'
    else:
        blastFormat = outFormat
        outFormat = 'out'
    if outName == None:
        #set the output name of the file if not defined
        dbName = os.path.basename(dbPath)
        flds = dbName.split('.')
        if len(flds) > 1:
            dbName = flds[0]
        queryName = os.path.basename(querySeq)
        flds = queryName.split('.')
        if len(flds) > 1:
            queryName = flds[0]
        #set the output path
        outName = '%s_vs_%s.%s'%(queryName,dbName, outFormat)
    if debug:
        print('OUTPUT FILE:\n%s%s'%(outDir, outName))
    #prepare the command
    #EXAMPLE blastn -query <input_seq.fa> -db <db_path> -evalue 0.001 -num_threads 32 -outfmt 0 -show_gis -out <out_file>
    #BIOPYTHON VERSION
    '''
    blastnCmd = NcbiblastnCommandline(query=querySeq, db=dbPath, evalue=eVal, outfmt=blastFormat, num_threads=threads, show_gis=True, out='%s%s'%(outDir, outName))
    if debug:
        print(blastnCmd)
    stdout, stderr = blastnCmd()
    '''
    blastnCmd = '%sblastn -query %s -db %s -evalue %s -outfmt %s -num_threads %s -show_gis -out %s%s'%(bin_root, querySeq, dbPath, str(eVal), str(blastFormat), str(threads), outDir, outName)
    if debug:
        print(blastnCmd)
    #execute the system call
    process = subprocess.Popen(blastnCmd, shell=True, stdout=subprocess.PIPE, stderr=subprocess.PIPE)
    stdout, stderr = process.communicate() #get stdout and stderr
    process.wait()
    if debug:
        print('blastn stdout:\n%s\n'%stdout)
        print('Blastn stderr:\n%s\n'%stderr)
    #return the output
    return(stdout, stderr, outDir+outName, str(blastnCmd))



def blastp(querySeq, dbPath, outDir=os.getcwd(), outName=None, eVal=0.001, outFormat='0', threads=4, debug=False):
    '''This function is used to execute blastp on query dna sequence.'''
    #from Bio.Blast.Applications import NcbiblastpCommandline
    #check that the input file and the database exist
    if not os.path.isfile(querySeq):
        sys.stderr.write('The file %s was not found, please provide the path to a valid FASTA file'%querySeq)
        sys.exit(-2)
    #check if the database exists
    dbFound= False
    if not (os.path.isfile(dbPath + '.pin') or os.path.isfile(dbPath + '.pal')): #.pal exists if the database has been split in many files
        sys.stderr.write('The database %s was not found, please create it using makeblastdb'%dbPath)
        sys.exit(-2)
    #print some debug
    if debug:
        print('Query:\t%s'%querySeq)
        print('Database:\t%s'%dbPath)
        print('Output dir:\t%s'%outDir)
        print('Output name:\t%s'%outName)
        print('Blastp evalue threshold:\t%s'%str(eVal))
        print('Output format:\t%s'%outFormat)
        print('Cores:\t%s'%str(threads))
    #set the output format
    blastFormat = None
    if (outFormat == 'xml') or (outFormat == '5'):
        blastFormat = '5'
        outFormat = 'xml'
    elif (outFormat == None) or (outFormat == '0'):
        blastFormat = '0'
        outFormat = 'out'
    elif (outFormat == 'txt') or (outFormat == '7'):
        blastFormat = '7'
        outFormat = 'out'
    elif outFormat == '11':
        blastFormat = '11'
        outFormat = 'asn'
    else:
        blastFormat = outFormat
        outFormat = 'out'
    #set output name
    if outName == None:
        #set the output name of the file if not defined
        dbName = os.path.basename(dbPath)
        flds = dbName.split('.')
        if len(flds) > 1:
            dbName = flds[0]
        queryName = os.path.basename(querySeq)
        flds = queryName.split('.')
        queryName = '.'.join(flds[:-1]) #remove extensions
        #set the output path
        outName = '%s_vs_%s.%s'%(queryName,dbName, outFormat)
    if debug:
        print('OUTPUT FILE:\n%s%s'%(outDir, outName))
    if debug:
        print('query:\t%s'%querySeq)
        print('db:\t%s'%dbPath)
        print('e-value:\t%s'%str(eVal))
        print('blast out format:\t%s'%str(blastFormat))
        print('output file extension:\t%s'%str(outFormat))
    #prepare the command
    #EXAMPLE blastp -query <input_seq.fa> -db <db_path> -evalue 0.001 -num_threads 32 -outfmt 0 -show_gis -out <out_file>
    #BIOPYTHON VERSION
    '''
    blastpCmd = NcbiblastpCommandline(query=querySeq, db=dbPath, evalue=eVal, outfmt=blastFormat, num_threads=threads, show_gis=True, out='%s%s'%(outDir, outName))
    stdout, stderr = blastpCmd() #using this will try to use the system blast installation
    '''
    blastpCmd = '%sblastp -query %s -db %s -evalue %s -outfmt %s -num_threads %s -show_gis -out %s%s'%(bin_root, querySeq, dbPath, str(eVal), str(blastFormat), str(threads), outDir, outName)
    if debug:
        print(blastpCmd)
    #execute the system call
    process = subprocess.Popen(blastpCmd, shell=True, stdout=subprocess.PIPE, stderr=subprocess.PIPE)
    stdout, stderr = process.communicate() #get stdout and stderr
    process.wait()
    if debug:
        print('Blastp stdout:\n%s\n'%stdout)
        print('Blastp stderr:\n%s\n'%stderr)
    #return the output
    return (stdout, stderr, outDir+outName, str(blastpCmd))



def blastx(querySeq, dbPath, outDir=os.getcwd(), outName=None, eVal=1e-3, outFormat='0', threads=4, debug=False):
    '''This function is used to execute blastp on query dna sequence.'''
    #from Bio.Blast.Applications import NcbiblastxCommandline
    #check that the input file and the database exist
    if not os.path.isfile(querySeq):
        sys.stderr.write('The file %s was not found, please provide the path to a valid FASTA file'%querySeq)
        sys.exit(-2)
    #check if the database exists
    dbFound= False
    if not (os.path.isfile(dbPath + '.pin') or os.path.isfile(dbPath + '.pal')): #.pal exists if the database has been split in many files
        sys.stderr.write('The database %s was not found, please create it using makeblastdb'%dbPath)
        sys.exit(-2)
    #print some debug
    if debug:
        print('\nblastx START:')
        print('Query:\t%s'%querySeq)
        print('Database:\t%s'%dbPath)
        print('Output dir:\t%s'%outDir)
        print('Output name:\t%s'%outName)
        print('Blastx evalue threshold:\t%s'%str(eVal))
        print('Output format:\t%s'%outFormat)
        print('Cores:\t%s'%str(threads))
    #set the output format
    blastFormat = None
    if (outFormat == 'xml') or (outFormat == '5'):
        blastFormat = '5'
        outFormat = 'xml'
    elif (outFormat == None) or (outFormat == '0'):
        blastFormat = '0'
        outFormat = 'out'
    elif (outFormat == 'txt') or (outFormat == '7'):
        blastFormat = '7'
        outFormat = 'out'
    elif outFormat == '11':
        blastFormat = '11'
        outFormat = 'asn'
    else:
        blastFormat = outFormat
        outFormat = 'out'
    #set output name
    if outName == None:
        #set the output name of the file if not defined
        dbName = os.path.basename(dbPath)
        flds = dbName.split('.')
        if len(flds) > 1:
            dbName = flds[0]
        queryName = os.path.basename(querySeq)
        flds = queryName.split('.')
        queryName = '.'.join(flds[:-1]) #remove extensions
        #set the output path
        outName = '%s_vs_%s.%s'%(queryName,dbName, outFormat)
    if debug:
        print('OUTPUT FILE:\n%s%s'%(outDir, outName))
    if debug:
        print('query:\t%s'%querySeq)
        print('db:\t%s'%dbPath)
        print('e-value:\t%s'%str(eVal))
        print('blast out format:\t%s'%str(blastFormat))
        print('output file extension:\t%s'%str(outFormat))
    #prepare the command
    #EXAMPLE blastx -query <input_seq.fa> -db <db_path> -evalue 0.001 -num_threads 32 -outfmt 0 -show_gis -out <out_file>
    #BIOPYTHON VERSION
    '''
    blastxCmd = NcbiblastxCommandline(query=querySeq, db=dbPath, evalue=eVal, outfmt=blastFormat, num_threads=threads, show_gis=True, out='%s%s'%(outDir, outName))
    if debug:
        print(blastxCmd)
    stdout, stderr = blastxCmd()
    '''
    blastxCmd = '%sblastx -query %s -db %s -evalue %s -outfmt %s -num_threads %s -show_gis -out %s%s'%(bin_root, querySeq, dbPath, str(eVal), str(blastFormat), str(threads), outDir, outName)
    if debug:
        print(blastxCmd)
    #execute the system call
    process = subprocess.Popen(blastxCmd, shell=True, stdout=subprocess.PIPE, stderr=subprocess.PIPE)
    stdout, stderr = process.communicate() #get stdout and stderr
    process.wait()
    if debug:
        print('Blastx stdout:\n%s\n'%stdout)
        print('Blastx stderr:\n%s\n'%stderr)
    #return the output
    return(stdout, stderr, outDir+outName, str(blastxCmd))



def convertBlastOutput(blastAsn, outDir=os.getcwd(), outName=None, outFormat='0', html=False, debug=False):
    '''
    Convert blast asn format output into a different blast output using blast_formatter.
    NOTE: the input archive must have generated using the -outfmt 11
    option with blastn or blastp
    '''
    if not os.path.isfile(blastAsn):
        sys.stderr.write('The file %s was not found, please provide the path to a valid blast asn archive file'%blastAsn)
        sys.exit(-2)
    #create the output directory
    systools.makedir(outDir)
    if debug:
        print('Input:\t%s'%blastAsn)
        print('Output dir:\t%s'%outDir)
        print('Output name:\t%s'%outName)
        print('Blast output format:\t%s'%outFormat)
        print('Generate HTML:\t%s'%html)
    #set the output format
    blastFormat = '0'
    outFormatStart = None
    if len(outFormat) > 1: #then it could be 2 digits
        outFormatStart = outFormat[:2] #extract first 2 characters
        outFormatStart = outFormatStart.strip()
    else:
        outFormatStart = outFormat[0].strip()
    validFormats = ['0', '1', '2', '3', '4', '5', '6', '7', '8', '9', '10', '12']
    if not outFormatStart in validFormats:
        sys.exit('format \'%s\' is not valid, please choose among one of the following\n%s\n'%(outFormatStart, str(validFormats)))
    if outFormatStart == '0':
        outExt = 'out'
    elif outFormatStart == '5':
        outExt = 'xml'
    elif outFormatStart == '8':
        outExt = 'asn.txt'
    elif outFormatStart == '9':
        outExt = 'asn.bin'
    elif outFormatStart == '10':
        outExt = 'csv'
    else:
        outExt = 'out'
    #set output name
    if outName == None:
        #set the output name of the file if not defined
        archvName = os.path.basename(blastAsn)
        flds = archvName.split('.')
        outName = '.'.join(flds[:-1])
        outName = '%s.%s'%(outName, outExt)
        if html:
            outName = '%s.html'%(outName)
    if debug:
        print('Output extension:\t%s'%(outExt))
        print('Output file:\n%s%s'%(outDir, outName))
    #prepare the command
    #EXAMPLE blast_formatter -archive <input.asn> -outfmt '6 qcov qcovhsp' -show_gis -out <out_file>
    #### EXTENDED OPTIONS ####
    '''
     *** Formatting options
     -outfmt <String>
       alignment view options:
         0 = pairwise,
         1 = query-anchored showing identities,
         2 = query-anchored no identities,
         3 = flat query-anchored, show identities,
         4 = flat query-anchored, no identities,
         5 = XML Blast output,
         6 = tabular,
         7 = tabular with comment lines,
         8 = Text ASN.1,
         9 = Binary ASN.1,
        10 = Comma-separated values,
        11 = BLAST archive format (ASN.1)
        12 = JSON Seqalign output
       Options 6, 7, and 10 can be additionally configured to produce
       a custom format specified by space delimited format specifiers.
       The supported format specifiers are:
           qseqid means Query Seq-id
              qgi means Query GI
             qacc means Query accesion
          qaccver means Query accesion.version
             qlen means Query sequence length
           sseqid means Subject Seq-id
        sallseqid means All subject Seq-id(s), separated by a ';'
              sgi means Subject GI
           sallgi means All subject GIs
             sacc means Subject accession
          saccver means Subject accession.version
          sallacc means All subject accessions
             slen means Subject sequence length
           qstart means Start of alignment in query
             qend means End of alignment in query
           sstart means Start of alignment in subject
             send means End of alignment in subject
             qseq means Aligned part of query sequence
             sseq means Aligned part of subject sequence
           evalue means Expect value
         bitscore means Bit score
            score means Raw score
           length means Alignment length
           pident means Percentage of identical matches
           nident means Number of identical matches
         mismatch means Number of mismatches
         positive means Number of positive-scoring matches
          gapopen means Number of gap openings
             gaps means Total number of gaps
             ppos means Percentage of positive-scoring matches
           frames means Query and subject frames separated by a '/'
           qframe means Query frame
           sframe means Subject frame
             btop means Blast traceback operations (BTOP)
          staxids means unique Subject Taxonomy ID(s), separated by a ';'
        (in numerical order)
       sscinames means unique Subject Scientific Name(s), separated by a ';'
        scomnames means unique Subject Common Name(s), separated by a ';'
       sblastnames means unique Subject Blast Name(s), separated by a ';'
        (in alphabetical order)
       sskingdoms means unique Subject Super Kingdom(s), separated by a ';'
        (in alphabetical order)
           stitle means Subject Title
       salltitles means All Subject Title(s), separated by a '<>'
          sstrand means Subject Strand
            qcovs means Query Coverage Per Subject
          qcovhsp means Query Coverage Per HSP
       When not provided, the default value is:
       'qseqid sseqid pident length mismatch gapopen qstart qend sstart send
       evalue bitscore', which is equivalent to the keyword 'std'
       Default = `0'
     -show_gis
       Show NCBI GIs in deflines?
     -num_descriptions <Integer, >=0>
       Number of database sequences to show one-line descriptions for
       Not applicable for outfmt > 4
       Default = `500'
        * Incompatible with:  max_target_seqs
     -num_alignments <Integer, >=0>
       Number of database sequences to show alignments for
       Default = `250'
        * Incompatible with:  max_target_seqs
     -line_length <Integer, >=1>
       Line length for formatting alignments
       Not applicable for outfmt > 4
       Default = `60'
     -html
       Produce HTML output?
    '''
    #create the command to be executed
    blastFmtCmd = '%s -archive %s -outfmt \'%s\' -show_gis -out %s '%(blastTools['blast_formatter'], blastAsn, outFormat, outDir+outName)
    if html:
        blastFmtCmd = '%s -html'%blastFmtCmd
    if debug:
        print(blastFmtCmd)
    #execute the system call
    process = subprocess.Popen(blastFmtCmd, shell=True, stdout=subprocess.PIPE, stderr=subprocess.PIPE)
    stdout_val, stderr_val = process.communicate() #get stdout and stderr
    process.wait()
    if debug:
        print('STDOUT:\n%s\n'%stdout_val)
        print('STDERR:\n%s\n'%stderr_val)
    #return a tuple with the results
    #blast_formatter cmd, new output, stdout, stderr
    return(blastFmtCmd, outDir+outName, stdout_val, stderr_val)



def extractBlastnBestHits(inBlastOut, outDir='%s/'%os.getcwd(), outName=None, outLog=False, debug=False):
    '''
    This function will parse a blast standard output file.
    NOTE: this parser has been tested only with
    default out (fmt=0) from blastn: 2.2.30+
    Package: blast 2.2.30, build Mon Apr 6 12:02:50 2015
    '''
    systools.makedir(outDir)
    #output file paths
    outTablePath = None
    inputBsname = os.path.basename(inBlastOut)
    if outName == None:
        outTableName = inputBsname.split('.')
        outTableName = '.'.join(outTableName[:-1])
        outTableName = '%s.blast.hits.dat'%outTableName
    else:
        outTableName = outName
    outTablePath = '%s%s'%(outDir, outTableName)
    #open the output file
    ofd = open(outTablePath, 'w')
    hdr = 'query\tsubjct\taln_len\tidentities\tgaps\thsp_len\tquery_aln_start\tquery_aln_end\tsubject_aln_start\tsubject_aln_end\tevalue\tblast_score\tidentity_pct\tquery_len\tsubject_len\tquery_pct_cov\tsubject_pct_cov\n'
    ofd.write(hdr)
    #open log file if needed
    outLogPath = lfd = None
    if outLog:
        outLogPath = '%sextractBlastnBestHits.log'%(outDir, )
        lfd = open(outLogPath, 'w')
        lfd.write('Input:\t%s'%inBlastOut)
        lfd.write('\nOutput dir:\t%s'%outDir)
        lfd.write('\nOutput name:\t%s'%outName)
        lfd.write('\nOutput table:\t%s'%outTablePath)
    ####### PARSE THE BLAST OUTPUT FILE ###########
    ctrlDict = {} # it will simply be used to understand if a db id has been repeated
    fd = open(inBlastOut, 'r')
    qHitOpen = False #used to see if we are in the part contaninig the hits
    readingHitDef = False #used to see if we are extracting the match title
    readingHsp = False #used to see if we are extracting the info and sequences of a given hsp
    qId = hId = ''
    bCnt = tblLnCnt = cnt = goodHspCnt = hspCnt = 0
    hspParseChk = 0 #will be used to see if all the hsp has been correctly extracted
    refSeqLen = qSeq = sbjSeq = matchSeq = qStart = qEnd = sbjStart = sbjEnd = qDir = sbjDir = identities = idPerc = gaps = alignLen = bScore = evalue = hId = qLen = hitLen = hspLen = None
    lnCnt = 0
    qCnt = 0
    for ln in fd:
        lnCnt += 1
        #print('\nline:\t%d'%lnCnt)
        #print(ln.rstrip())
        if ln.startswith('Query='):
            ln = ln[6:-1].strip() #remove the "Query="
            qCnt += 1
            if qHitOpen:
                #### WRITE THE RECORD IN THE OUTPUT FILE IF NOT WRITTEN YET ####
                if ((matchSeq != None) or (qCnt == 1)):
                    #check that the sequecens lengths are same
                    if not (len(qSeq) == len(sbjSeq) == len(matchSeq)):
                        print('QUERY SEQ LENGTH:\t%s'%len(qSeq))
                        print('MATCH SEQ LENGTH:\t%s'%len(matchSeq))
                        print('SBJ SEQ LENGTH:\t%s'%len(sbjSeq))
                        sys.exit('ERROR: the length of all the  extracted sequences must be the same')
                    qCovHsp = round((float(hspLen)/float(qLen))*100, 2)
                    sbjCovHsp = round((float(hspLen)/float(hitLen))*100, 2)
                    if debug:
                        print('QUERY LENGTH:\t%s'%qLen)
                        print('SUBJECT LENGTH:\t%s'%hitLen)
                        print('ALIGN LENGTH:\t%s'%alignLen)
                        print('QCOVHSP:\t%s'%qCovHsp)
                        print('SBJCOVHSP:\t%s'%sbjCovHsp)
                    #ADD THE RECORD TO THE DATABASE
                    outStr = '%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s\n'%(qId, hId, str(alignLen), str(identities), str(gaps), str(hspLen), qStart, qEnd, sbjStart, sbjEnd, str(evalue), str(bScore), idPerc, qLen, hitLen, qCovHsp, sbjCovHsp)
                    ##### WRITE LINE IN OUTPUT TABLE #####
                    ofd.write(outStr)
                    tblLnCnt += 1
                    if debug:
                        print('\nOUTPUT STRING:\n%s'%outStr)
                    #sys.exit('debug')
                    matchSeq = None #reset the variable
                    outStr = None
                    #reset main variables
                    hitLen = refSeqLen = qSeq = sbjSeq = matchSeq = qStart = qEnd = sbjStart = sbjEnd = qDir = sbjDir = identities = idPerc = gaps = alignLen = bScore = evalue = hspLen = None
                    ##################################
                readingHsp = qHitOpen = False
                if debug:
                    print('%s GOOD HITS FOR QUERY %s'%(str(goodHspCnt), qId))
                    print('%s EXTRACTED HITS FOR QUERY %s\n'%(str(hspParseChk), qId))
                    print('%s TOTAL HSP FOR QUERY %s'%(str(hspCnt), qId))
                    print('TOTAL RECORDS IN OUTPUT TABLE %s'%(str(tblLnCnt)))
            #check that all the hsps had been extracted
                if goodHspCnt != hspParseChk:
                    if debug:
                        print('GOOD HITS %s'%(str(goodHspCnt)))
                        print('EXTRACTED HITS %s'%(str(hspParseChk)))
                        print('TOTAL HSP %s\n'%(str(hspCnt)))
                    sys.exit('Some of the hsp had not been extracted...\nhsp found: %d\thsp extrated: %d'%(goodHspCnt, hspParseChk))
                else:
                    if debug:
                        print('\nOK!\n')
                #sys.exit('debug :: qhitopen end')
            hspParseChk = goodHspCnt = hspCnt = 0
            qId = ln
        #extract query length
        elif (ln[:7] == 'Length=') and (not readingHitDef):
            qLen = int(ln[7:-1])
            if debug:
                print('QUERY LENGTH: %s'%(qLen))
        elif ln[0] == '*': #NO HITS FOR THE QUERY
            #WRITE SPECIAL LINE
            outStr = '%s\tNaN\tNaN\tNaN\tNaN\tNaN\tNaN\tNaN\tNaN\tNaN\tNaN\tNaN\tNaN\tNaN\tNaN\tNaN\tNaN\n'%(qId)
            ##### WRITE LINE IN OUTPUT TABLE #####
            ofd.write(outStr)
            tblLnCnt += 1
            if debug:
                print('\nORIGINAL:\n%s'%outStr)
            #sys.exit('debug')
            #reset main variables
            matchSeq = None #reset the variable
            outStr = None
            hitLen = refSeqLen = qSeq = sbjSeq = matchSeq = qStart = qEnd = sbjStart = sbjEnd = qDir = sbjDir = identities = idPerc = gaps = alignLen = bScore = evalue = hspLen = None
            #reset flags
            HspCnt = 0
            qHitOpen = False
            readingHitDef = False
        elif ln.startswith('Sequences'): #start counting the number of HSP
            if debug:
                print(qId)
            qHitOpen = True
            continue
        #USE THE FLAGS TO EXTRACT INFO
        if qHitOpen:
            if (ln[:2] == '  ') and (hspParseChk == 0):
                if debug:
                    print(ln.strip())
                goodHspCnt += 1
            elif ln[0] == '>': #then we found an hsp (hit)
                #WRITE THE LINE IF THE INFORMATION ARE AVAILABLE
                if (matchSeq != None): #if it is the first query then write down the line in the output file
                    #check that the sequecens lengths are same
                    if not (len(qSeq) == len(sbjSeq) == len(matchSeq)):
                        print('QUERY SEQ LENGTH:\t%s'%len(qSeq))
                        print('MATCH SEQ LENGTH:\t%s'%len(matchSeq))
                        print('SBJ SEQ LENGTH:\t%s'%len(sbjSeq))
                        sys.exit('ERROR: the length of all the  extracted sequences must be the same')
                    qCovHsp = round((float(hspLen)/float(qLen))*100, 2)
                    sbjCovHsp = round((float(hspLen)/float(hitLen))*100, 2)
                    if debug:
                        print('QUERY LENGTH:\t%s'%qLen)
                        print('SUBJECT LENGTH:\t%s'%hitLen)
                        print('ALIGN LENGTH:\t%s'%alignLen)
                        print('QCOVHSP:\t%s'%qCovHsp)
                        print('SBJCOVHSP:\t%s'%sbjCovHsp)
                    #ADD THE RECORD TO THE DATABASE
                    outStr = '%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s\n'%(qId, hId, str(alignLen), str(identities), str(gaps), str(hspLen), qStart, qEnd, sbjStart, sbjEnd, str(evalue), str(bScore), idPerc, qLen, hitLen, qCovHsp, sbjCovHsp)
                    ##### WRITE LINE IN OUTPUT TABLE #####
                    ofd.write(outStr)
                    tblLnCnt +=1
                    if debug:
                        print('\nOUTPUT STRING:\n%s'%outStr)
                    outStr = None
                #reset main variables
                hitLen = refSeqLen = qSeq = sbjSeq = matchSeq = qStart = qEnd = sbjStart = sbjEnd = qDir = sbjDir = identities = idPerc = gaps = alignLen = bScore = evalue = hspLen = None
                if debug:
                    print('\n\n##### HIT FOUND ######')
                hId = ln[2:-1]
                hspParseChk += 1
                readingHitDef = True
                readingHsp = False
                continue
            #let's now keep reading the hit name
            if readingHitDef: #extract the hit length
                if ln.startswith('Length='):
                    hitLen = int(ln[7:-1])
                    readingHitDef = False
                    if debug:
                        print('HIT LEN:\t%d\n'%hitLen)
                else: #let's add other parts of the hit definition
                    hId = hId + ln[0:-1]
                    if debug:
                        print('HIT:\t%s\n'%hId)
            else: #let's extract the alignments and other info
                if ln[:6] == ' Score': #then we found the line with Score, bitscore and evalue
                    outStr = None
                    if debug:
                        print('\n##### HSP SCORE FOUND ######')
                    if (matchSeq != None): #if it is the first query then write down the line in the output file
                        #check that the sequecens lengths are same
                        if not (len(qSeq) == len(sbjSeq) == len(matchSeq)):
                            print('QUERY SEQ LENGTH:\t%s'%len(qSeq))
                            print('MATCH SEQ LENGTH:\t%s'%len(matchSeq))
                            print('SBJ SEQ LENGTH:\t%s'%len(sbjSeq))
                            sys.exit('ERROR: the length of all the  extracted sequences must be the same')
                        qCovHsp = round((float(hspLen)/float(qLen))*100, 2)
                        sbjCovHsp = round((float(hspLen)/float(hitLen))*100, 2)
                        if debug:
                            print('QUERY LENGTH:\t%s'%qLen)
                            print('SUBJECT LENGTH:\t%s'%hitLen)
                            print('ALIGN LENGTH:\t%s'%alignLen)
                            print('QCOVHSP:\t%s'%qCovHsp)
                            print('SBJCOVHSP:\t%s'%sbjCovHsp)
                        ##### WRITE LINE IN OUTPUT TABLE #####
                        outStr = '%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s\n'%(qId, hId, str(alignLen), str(identities), str(gaps), str(hspLen), qStart, qEnd, sbjStart, sbjEnd, str(evalue), str(bScore), idPerc, qLen, hitLen, qCovHsp, sbjCovHsp)
                        ofd.write(outStr)
                        tblLnCnt +=1
                        if debug:
                            print('\nOUTPUT STRING:\n%s'%outStr)
                        outStr = None
                    #reset main variables
                    refSeqLen = qSeq = sbjSeq = matchSeq = qStart = qEnd = sbjStart = sbjEnd = qDir = sbjDir = identities = idPerc = gaps = alignLen = bScore = evalue = hspLen = None
                    readingHsp = True #then we are reading the hsp and sequences
                    hspCnt += 1
                    flds = ln.split(' = ')
                    #extract the bitscore
                    bScore = flds[1]
                    bScore = bScore.split(' bits')
                    bScore = float(bScore[0].strip())
                    #now extract the evalue
                    evalue = flds[-1].strip()
                    if debug:
                        print('B-SCORE\t%s'%str(bScore))
                        print('E-VALUE\t%s'%str(evalue))
                    #print('qHitOpen:%s\treadingHitDef:%s\treadingHsp:%s'%(qHitOpen, readingHitDef, readingHsp))
                    #sys.exit('debug::Score')
                    continue
                if readingHsp:
                    if ln.startswith(' Identities'): #then we found the line with Identities, Identity Perc and Gaps
                        flds = ln.split(' = ')
                        #extract the bitscore
                        tmpFlds = flds[1].split('(') #then we need to remove the dx part to get the bitscore
                        #identity percentage
                        idPerc = tmpFlds[-1]
                        tmpFlds2 = idPerc.split('%')
                        idPerc = tmpFlds2[0].strip()
                        #identitites and alignLength
                        tmpFlds2 = tmpFlds[0].split('/')
                        identities = int(tmpFlds2[0].strip())
                        alignLen = int(tmpFlds2[-1].strip())
                        del tmpFlds2
                        #gaps
                        gaps = flds[-1].split('/')
                        gaps = int(gaps[0].strip())
                        hspLen = alignLen - gaps
                        del flds
                        #print('qHitOpen:%s\treadingHitDef:%s\treadingHsp:%s'%(qHitOpen, readingHitDef, readingHsp))
                        #sys.exit('debug::Identities')
                    elif ln[:7] == ' Strand': #then we found the line the strands
                        tmpLn = ln[8:-1]
                        flds = tmpLn.split('/')
                        qDir = sbjDir = ''
                        if flds[0][0] == 'P': #then it is plus
                            qDir = '+'
                        else: qDir = '-'
                        #now the direction for the hit
                        if flds[1][0] == 'P': #then it is plus
                            sbjDir = '+'
                        else: sbjDir = '-'
                        if debug:
                            print('STRAND DIRECTIONS: %s/%s'%(qDir, sbjDir))
                        #print('qHitOpen:%s\treadingHitDef:%s\treadingHsp:%s'%(qHitOpen, readingHitDef, readingHsp))
                        #sys.exit('debug::Strand')
                    #extract the sequences and matches
                    elif ln.startswith('Query '):
                        #remove the 'Query  ' part
                        ln = ln[7:-1]
                        flds = ln.split('  ')
                        if qStart == None:
                            qStart = int(flds[0].strip())
                        qEnd = int(flds[-1].strip())
                        refSeqLen = len(flds[-2].strip()) #this will be used as a reference for the length in order to extract the '|' describing the match
                        if qSeq != None:
                            qSeq = qSeq + flds[-2].strip()
                        else:
                            qSeq = flds[-2].strip()
                        #if debug:
                            #print('len=%s %s %s %s'%(len(qSeq), qStart, qSeq, qEnd))
                        #print('qHitOpen:%s\treadingHitDef:%s\treadingHsp:%s'%(qHitOpen, readingHitDef, readingHsp))
                        #sys.exit('debug::Extract alignments (query and subjects)')
                    #extract the matches and gaps
                    elif  ('|' in ln) and ('>' not in ln):
                        #if debug:
                            #print('REFERENCE LENGTH: %s'%refSeqLen)
                        if matchSeq != None:
                            matchSeq = matchSeq + ln[-refSeqLen-1:].rstrip('\n')
                        else:
                            matchSeq = ln[-refSeqLen-1:].rstrip('\n')
                        #if debug:
                            #print('len=%s %s'%(len(matchSeq), matchSeq))
                        #print('qHitOpen:%s\treadingHitDef:%s\treadingHsp:%s'%(qHitOpen, readingHitDef, readingHsp))
                        #sys.exit('debug::Extract pipes \'|\' for alignments')
                    #extract the hit sequence
                    elif ln[:5] == 'Sbjct':
                        #remove the 'Sbjct  ' part
                        ln = ln[7:-1]
                        flds = ln.split('  ')
                        if sbjStart == None:
                            sbjStart = int(flds[0].strip())
                        sbjEnd = int(flds[-1].strip())
                        if sbjSeq != None:
                            sbjSeq = sbjSeq + flds[-2].strip()
                        else:
                            sbjSeq = flds[-2].strip()
                        #if debug:
                            #print('len=%s %s %s %s'%(len(sbjSeq), sbjStart, sbjSeq, sbjEnd))
                        #print('qHitOpen:%s\treadingHitDef:%s\treadingHsp:%s'%(qHitOpen, readingHitDef, readingHsp))
                        #sys.exit('debug::Extract subject sequence matches')
    #write the last hsp
    if matchSeq != None:
        #check that the sequecens lengths are same
        if not (len(qSeq) == len(sbjSeq) == len(matchSeq)):
            print('QUERY SEQ LENGTH:\t%s'%len(qSeq))
            print('MATCH SEQ LENGTH:\t%s'%len(matchSeq))
            print('SBJ SEQ LENGTH:\t%s'%len(sbjSeq))
            sys.exit('ERROR: the length of all the  extracted sequences must be the same')
        qCovHsp = round((float(hspLen)/float(qLen))*100, 2)
        sbjCovHsp = round((float(hspLen)/float(hitLen))*100, 2)
        if debug:
            print('QUERY LENGTH:\t%s'%qLen)
            print('SUBJECT LENGTH:\t%s'%hitLen)
            print('ALIGN LENGTH:\t%s'%alignLen)
            print('QCOVHSP:\t%s'%qCovHsp)
            print('SBJCOVHSP:\t%s'%sbjCovHsp)
        ##### WRITE LINE IN OUTPUT TABLE #####
        outStr = '%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s\n'%(qId, hId, str(alignLen), str(identities), str(gaps), str(hspLen), qStart, qEnd, sbjStart, sbjEnd, str(evalue), str(bScore), idPerc, qLen, hitLen, qCovHsp, sbjCovHsp)
        ofd.write(outStr)
        tblLnCnt +=1
        if debug:
            print('\nOUTPUT STRING:\n%s'%outStr)
    #CLOSE MAIN OUTPUT
    ofd.close()
    #CLOSE THE LOG FILE IF NEEDED
    if lfd:
        lfd.close()
    return outTablePath



def extractBlastnBestHitsKc(inBlastOut, outDir=os.getcwd(), bestHitsLimit=2, outPrefix=None, outLog=False, debug=False):
    '''
    This script will take as input a text file generated using blastn (from BLAST+), and extract the best hits for
    each pair query-subject DNA sequence.
    It relies on a kyotocabinet databases containing taxonomic information
    '''
    import shutil
    limitExceeded = False #used to decide if the hit should be counted or not
    multiAlign = False #used to understand if we are reading a secondary alignment for the same pair (query, subject)
    #write some debug about the input and the program
    if debug:
        sys.stdout.write('\nINPUT:%s\n'%inBlastOut)
        sys.stdout.write('OUTPUT DIRECTORY:%s\n'%outDir)
        sys.stdout.write('MAX HITS PER QUERY:%s\n'%str(bestHitsLimit))
        sys.stdout.write('OUTPUT LOG FILE:\t%s\n'%outLog)
    #set database extension
    dbExt = 'kch'
    #check that input file and output directory exist
    if not os.path.isfile(inBlastOut):
        sys.stderr.write('The file %s was not found, please provide the path to a valid blastn output file'%inBlastOut)
        sys.exit(-2)
    #create the output directory
    import sys_tools as systools
    systools.makedir(outDir)
    import kyotocabinet as kc
    #set main variables
    hitCnt = 0
    totHitCnt = 0
    missingHitCnt = 0
    noHitCnt = 0
    dbdate = '20141215' #ncbi taxonomy database version
    #path where the leveldb database is located
    dbdir = '/user/gen-info/salvocos/projects/dbs/local/ncbi_taxonomy/kyotocabinet/%s/'%dbdate
    #set the paths to the databases
    dbExt = 'kct'
    gidb = '%sgi_taxid_nucl.%s'%(dbdir, dbExt)
    dbExt = 'kch'
    #Database path
    nodesdb = '%snodes_taxid_parent-taxid.%s'%(dbdir, dbExt)
    namesdb = '%snames_sci.%s'%(dbdir, dbExt)
    ranksdb = '%snodes_taxid2rank.%s'%(dbdir, dbExt)
    maxHitDict = {} #will contain a counter for each hits
    ### OPEN THE DATABASES ###
    gidbHandle = kc.DB()
    gidbHandle.open(gidb, kc.DB.OREADER | kc.DB.ONOLOCK)
    nodesdbHandle = kc.DB()
    nodesdbHandle.open(nodesdb, kc.DB.OREADER | kc.DB.ONOLOCK)
    namesdbHandle = kc.DB()
    namesdbHandle.open(namesdb, kc.DB.OREADER | kc.DB.ONOLOCK)
    ranksdbHandle = kc.DB()
    ranksdbHandle.open(ranksdb, kc.DB.OREADER | kc.DB.ONOLOCK)
    if debug: #print db paths
        print('gidbHandle :: %s'%gidb)
        print('nodesdbHandle :: %s'%nodesdb)
        print('namesdbHandle :: %s'%namesdb)
        print('ranksdbHandle :: %s'%ranksdb)
    #########################
    #define output file paths
    topHitPath = noHitPath = missingHitPath = summaryPath = None
    if outPrefix != None:
        topHitPath = '%s%s.tophit.list.txt'%(outDir, outPrefix)
        noHitPath = '%s%s.nohit-id.list.txt'%(outDir, outPrefix)
        missingHitPath = '%s%s.not_in_db-id.list.txt'%(outDir, outPrefix)
        summaryPath = '%s%s.hit.stats.txt'%(outDir, outPrefix)
    else:
        topHitPath = '%s%s.tophit.list.txt'%(outDir, os.path.basename(inBlastOut))
        noHitPath = '%s%s.nohit-id.list.txt'%(outDir, os.path.basename(inBlastOut))
        missingHitPath = '%s%s.not_in_db-id.list.txt'%(outDir, os.path.basename(inBlastOut))
        summaryPath = '%s%s.hit.stats.txt'%(outDir, os.path.basename(inBlastOut))
    if debug:
        sys.stdout.write('\nTOP HIT OUTPUT FILE:%s\n'%topHitPath)
        sys.stdout.write('NO HIT OUTPUT FILE:%s\n'%noHitPath)
        sys.stdout.write('NOT PRESENT IN DATABASE OUTPUT FILE:%s\n'%missingHitPath)
        sys.stdout.write('SUMMARY FILE:%s\n'%summaryPath)
    #start parsing the blasn output file and open the 2 output files
    noHitFd = open(noHitPath, 'w')
    topHitFd = open(topHitPath, 'w')
    missHitFd = open(missingHitPath, 'w')
    query = subject = None #will contain the query id
    #write hdr in the top hits file
    topHitFd.write('QueryID\tHit_gi\tTaxID\tSuperkingdom\tPhylum\tClass\tOrder\tFamily\tGenus\tSpecies\tScore\tE-value\n')
    fd = open(inBlastOut)
    loopCnt = 0
    loopLimit = 400
    for ln in fd:
        loopCnt +=1
        ln = ln.rstrip()
        if ln == '': continue
        elif ln[:7] == 'Query= ': #make sure that the extra space is present also in older blastn versions
            query = ln[7:]
            if debug:
                print('line:\t%s\n'%ln)
        elif 'No hits found' in ln:
            noHitCnt +=1
            noHitFd.write('%s\n'%query)
            totHitCnt +=1
            if debug:
                print('line:\t%s\n'%ln)
        elif ln[0] == '>': #it is a it so we should write it in the top hits list
            if debug:
                print('line:\t%s\n'%ln)
            subject = ln[1:] #store the hit line
            multiAlign = False #reset the variable since we just found this hit sequence
            #add the query to the dictionary with the count and increment the counter
            if not query in maxHitDict:
                limitExceeded = False
                maxHitDict[query] = 1
            else: #just increment the counter
                if maxHitDict[query] + 1 > bestHitsLimit:
                    limitExceeded = True
                    continue
                else: maxHitDict[query] += 1
            #this is dictinary will contain the final string
            hitDict = OrderedDict()
            hitDict['QueryID'] = query
            hitCnt +=1
            totHitCnt +=1
            #extract gi parts
            flds = ln.split('|')
            gi = flds[1].strip()
            if debug:
                print('gi:\t%s'%gi)
            taxid = None
            #check that gi is in the gidb database (handle the key-error exception)
            try:
                #taxid = gidbHandle.Get(gi)
                taxid = gidbHandle.get(gi)
                hitDict['Hit_gi'] = gi
                hitDict['TaxID'] = taxid
                if debug:
                    print('gi:\t%s'%gi)
                    print('taxid:\t%s'%taxid)
            except KeyError:
                if debug:
                    print('WARNING: %s is not a valid taxid'%gi)
                if taxid == None:
                    raise
            #add the other fields to the dictionary as None
            hitDict['Superkingdom'] = hitDict['Phylum'] = hitDict['Class'] = hitDict['Order'] = hitDict['Family'] = hitDict['Genus'] = hitDict['Species'] = hitDict['Score'] = hitDict['E-value'] = None
            #get the node in the taxonomy tree
            taxnode = rank = None
            try:
                #taxnode = None
                taxnode = nodesdbHandle.get(taxid)
                if debug:
                    print('taxnode:\t%s'%taxnode)
            except KeyError:
                if debug:
                    print('WARNING: %s is not a valid taxonomy node'%taxid)
                if taxid == None:
                    raise
            #let's now loop to find all the taxonomic levels
            if taxnode != None:
                #set the values in the dictionary
                while nodesdbHandle.get(taxid) != taxid:
                    hitDict[ranksdbHandle.get(taxid).title()] = namesdbHandle.get(taxid)
                    #update the values to update the while's condition outcome
                    taxid = nodesdbHandle.get(taxid)
                    #sys.exit('DEBUG: taxnode!= None')
            else:#write the line in the file with not found
                missHitFd.write('%s\n'%ln)
                if debug:
                    print(ln)
                missingHitCnt +=1
                hitCnt -=1
                #sys.exit('debug')
                continue
            #CHECK THE DICTIONARY CONTENT
            for k in hitDict:
                if debug:
                    print('%s::\t%s'%(k, hitDict[k]))
        elif ln[:8] == ' Score =': #this line contain sthe score and e value
            if debug:
                print('Score line:\t%s\n'%ln)
            #if too many lines for the query has been printed, than skipt it
            if limitExceeded:
                if debug:
                    print('%s hits had been printed for query %s, while the max limit is %s'%(str(maxHitDict[query]), query, str(bestHitsLimit)))
                continue
            #avoid writing multiple hits for the same couple (query, subject)
            if multiAlign:
                if debug:
                    print('a hit line was previously printed for query %s and hit sequence %s'%(query, subject))
                continue
            flds = ln.split(' bits ') #left parts is score right part is E-value
            score = flds[0][8:] #remove the 'Score =' part...
            score = float(score.strip())
            hitDict['Score'] = score
            #let's now extract the e-value
            evalue = flds[-1].split('Expect = ')
            evalue = evalue[-1].strip()
            hitDict['E-value'] = evalue
            if debug:
                print('SCORE:\t%s'%str(score))
                print('E-VALUE:\t%s'%evalue)
            #PREPARE THE FINAL LINE
            tmpList = []
            for k in hitDict:
                tmpList.append(str(hitDict[k]))
                if k == 'E-value':
                    break
            #PRINT THE LINE UNTIL THE E-VALUE
            topHitFd.write('%s\n'%'\t'.join(tmpList))
            multiAlign = True #avoid writing multiple hits for the same couple (query, subject)
            tmpList = None
    ### CLOSE THE DATABASES ###
    gidbHandle.close()
    nodesdbHandle.close()
    namesdbHandle.close()
    ranksdbHandle.close()
    #remove local database
    #shutil.rmtree(localDbDir, ignore_errors=True)
    #close input and output files
    fd.close()
    noHitFd.close()
    topHitFd.close()
    missHitFd.close()
    #OUTPUT SUMMARY
    summaryFd = open(summaryPath, 'w')
    summaryFd.write('\nINPUT:%s\n'%inBlastOut)
    summaryFd.write('OUTPUT DIRECTORY:%s\n'%outDir)
    summaryFd.write('OUTPUT LOG FILE:\t%s\n'%outLog)
    summaryFd.write('MAX HITS PER QUERY:%s\n'%str(bestHitsLimit))
    summaryFd.write('TOP HIT OUTPUT FILE:%s\n'%topHitPath)
    summaryFd.write('NO HIT OUTPUT FILE:%s\n'%noHitPath)
    summaryFd.write('NOT PRESENT IN DATABASE OUTPUT FILE:%s\n'%missingHitPath)
    summaryFd.write('SUMMARY FILE:%s\n'%summaryPath)
    summaryFd.write('\nTOT HITS:\t%s\n'%str(totHitCnt))
    summaryFd.write('FOUND IN DB:\t%s\n'%str(hitCnt))
    summaryFd.write('NOT IN DB:\t%s\n'%str(missingHitCnt))
    summaryFd.write('NO HITS FOUND:\t%s\n'%str(noHitCnt))
    summaryFd.close()
    #return output files paths
    return(topHitPath, noHitPath, missingHitPath, summaryPath)



def extractBlastnBestHitsKcVirus(inBlastOut, outDir=os.getcwd(), bestHitsLimit=2, outPrefix=None, outLog=False, debug=False):
    '''
    This script will take as input a text file generated using blastn (from BLAST+), and extract the best hits for
    each pair query-subject DNA sequence.
    It relies on a kyotocabinet databases containing taxonomic information
    '''
    import shutil
    limitExceeded = False #used to decide if the hit should be counted or not
    multiAlign = False #used to understand if we are reading a secondary alignment for the same pair (query, subject)
    #write some debug about the input and the program
    if debug:
        sys.stdout.write('\nINPUT:%s\n'%inBlastOut)
        sys.stdout.write('OUTPUT DIRECTORY:%s\n'%outDir)
        sys.stdout.write('MAX HITS PER QUERY:%s\n'%str(bestHitsLimit))
        sys.stdout.write('OUTPUT LOG FILE:\t%s\n'%outLog)
    #set database extension
    dbExt = 'kch'
    #check that input file and output directory exist
    if not os.path.isfile(inBlastOut):
        sys.stderr.write('The file %s was not found, please provide the path to a valid blastn output file'%inBlastOut)
        sys.exit(-2)
    #create the output directory
    import sys_tools as systools
    systools.makedir(outDir)
    import kyotocabinet as kc
    #set main variables
    hitCnt = 0
    totHitCnt = 0
    missingHitCnt = 0
    noHitCnt = 0
    dbdate = '20150911_virus' #ncbi taxonomy database version
    #path where the leveldb database is located
    dbdir = '/user/gen-info/salvocos/projects/dbs/local/ncbi_taxonomy/kyotocabinet/%s/'%dbdate
    #set the paths to the databases
    dbExt = 'kct'
    gidb = '%sgi_taxid_nucl_v.%s'%(dbdir, dbExt)
    #dbExt = 'kch'
    #Database path
    nodesdb = '%snodes_taxid2parent-taxid.%s'%(dbdir, dbExt)
    namesdb = '%snames_sci.%s'%(dbdir, dbExt)
    ranksdb = '%snodes_taxid2rank.%s'%(dbdir, dbExt)
    maxHitDict = {} #will contain a counter for each hits
    ### OPEN THE DATABASES ###
    gidbHandle = kc.DB()
    gidbHandle.open(gidb, kc.DB.OREADER | kc.DB.ONOLOCK)
    nodesdbHandle = kc.DB()
    nodesdbHandle.open(nodesdb, kc.DB.OREADER | kc.DB.ONOLOCK)
    namesdbHandle = kc.DB()
    namesdbHandle.open(namesdb, kc.DB.OREADER | kc.DB.ONOLOCK)
    ranksdbHandle = kc.DB()
    ranksdbHandle.open(ranksdb, kc.DB.OREADER | kc.DB.ONOLOCK)
    if debug: #print db paths
        print('gidbHandle :: %s'%gidb)
        print('nodesdbHandle :: %s'%nodesdb)
        print('namesdbHandle :: %s'%namesdb)
        print('ranksdbHandle :: %s'%ranksdb)
    #########################
    #define output file paths
    topHitPath = noHitPath = missingHitPath = summaryPath = None
    if outPrefix != None:
        topHitPath = '%s%s.tophit.virus.list.txt'%(outDir, outPrefix)
        noHitPath = '%s%s.nohit-id.virus.list.txt'%(outDir, outPrefix)
        missingHitPath = '%s%s.not_in_db-id.virus.list.txt'%(outDir, outPrefix)
        summaryPath = '%s%s.hit.virus.stats.txt'%(outDir, outPrefix)
    else:
        topHitPath = '%s%s.tophit.virus.list.txt'%(outDir, os.path.basename(inBlastOut))
        noHitPath = '%s%s.nohit-id.virus.list.txt'%(outDir, os.path.basename(inBlastOut))
        missingHitPath = '%s%s.not_in_db-id.virus.list.txt'%(outDir, os.path.basename(inBlastOut))
        summaryPath = '%s%s.hit.virus.stats.txt'%(outDir, os.path.basename(inBlastOut))
    if debug:
        sys.stdout.write('\nTOP HIT OUTPUT FILE:%s\n'%topHitPath)
        sys.stdout.write('NO HIT OUTPUT FILE:%s\n'%noHitPath)
        sys.stdout.write('NOT PRESENT IN DATABASE OUTPUT FILE:%s\n'%missingHitPath)
        sys.stdout.write('SUMMARY FILE:%s\n'%summaryPath)
    #start parsing the blasn output file and open the 2 output files
    noHitFd = open(noHitPath, 'w')
    topHitFd = open(topHitPath, 'w')
    missHitFd = open(missingHitPath, 'w')
    query = subject = None #will contain the query id
    #write hdr in the top hits file
    topHitFd.write('QueryID\tHit_gi\tTaxID\tSuperkingdom\tPhylum\tClass\tOrder\tFamily\tGenus\tSpecies\tScore\tE-value\n')
    fd = open(inBlastOut)
    loopCnt = 0
    loopLimit = 400
    for ln in fd:
        loopCnt +=1
        ln = ln.rstrip()
        if ln == '': continue
        elif ln[:7] == 'Query= ': #make sure that the extra space is present also in older blastn versions
            query = ln[7:]
            if debug:
                print('line:\t%s\n'%ln)
        elif 'No hits found' in ln:
            noHitCnt +=1
            noHitFd.write('%s\n'%query)
            totHitCnt +=1
            if debug:
                print('line:\t%s\n'%ln)
        elif ln[0] == '>': #it is a it so we should write it in the top hits list
            if debug:
                print('line:\t%s\n'%ln)
            subject = ln[1:] #store the hit line
            multiAlign = False #reset the variable since we just found this hit sequence
            #add the query to the dictionary with the count and increment the counter
            if not query in maxHitDict:
                limitExceeded = False
                maxHitDict[query] = 1
            else: #just increment the counter
                if maxHitDict[query] + 1 > bestHitsLimit:
                    limitExceeded = True
                    continue
                else: maxHitDict[query] += 1
            #this is dictinary will contain the final string
            hitDict = OrderedDict()
            hitDict['QueryID'] = query
            hitCnt +=1
            totHitCnt +=1
            #extract gi parts
            flds = ln.split('|')
            gi = flds[1].strip()
            if debug:
                print('gi:\t%s'%gi)
            taxid = None
            #check that gi is in the gidb database (handle the key-error exception)
            try:
                #taxid = gidbHandle.Get(gi)
                taxid = gidbHandle.get(gi)
                hitDict['Hit_gi'] = gi
                hitDict['TaxID'] = taxid
                if debug:
                    print('gi:\t%s'%gi)
                    print('taxid:\t%s'%taxid)
            except KeyError:
                if debug:
                    print('WARNING: %s is not a valid taxid'%gi)
                if taxid == None:
                    raise
            #add the other fields to the dictionary as None
            hitDict['Superkingdom'] = hitDict['Phylum'] = hitDict['Class'] = hitDict['Order'] = hitDict['Family'] = hitDict['Genus'] = hitDict['Species'] = hitDict['Score'] = hitDict['E-value'] = None
            #get the node in the taxonomy tree
            taxnode = rank = None
            try:
                #taxnode = None
                taxnode = nodesdbHandle.get(taxid)
                if debug:
                    print('taxnode:\t%s'%taxnode)
            except KeyError:
                if debug:
                    print('WARNING: %s is not a valid taxonomy node'%taxid)
                if taxid == None:
                    raise
            #let's now loop to find all the taxonomic levels
            if taxnode != None:
                #set the values in the dictionary
                while nodesdbHandle.get(taxid) != taxid:
                    if debug:
                        print('\n#')
                        print(taxnode)
                        print(taxid)
                    if taxid == '1':
                        taxid = nodesdbHandle.get(taxid)
                        continue
                    hitDict[ranksdbHandle.get(taxid).title()] = namesdbHandle.get(taxid)
                    #update the values to update the while's condition outcome
                    taxid = nodesdbHandle.get(taxid)
                    #sys.exit('DEBUG: taxnode!= None')
            else:#write the line in the file with not found
                missHitFd.write('%s\n'%ln)
                if debug:
                    print(ln)
                missingHitCnt +=1
                hitCnt -=1
                #sys.exit('debug')
                continue
            #CHECK THE DICTIONARY CONTENT
            for k in hitDict:
                if debug:
                    print('%s::\t%s'%(k, hitDict[k]))
        elif ln[:8] == ' Score =': #this line contain sthe score and e value
            if debug:
                print('Score line:\t%s\n'%ln)
            #if too many lines for the query has been printed, than skipt it
            if limitExceeded:
                if debug:
                    print('%s hits had been printed for query %s, while the max limit is %s'%(str(maxHitDict[query]), query, str(bestHitsLimit)))
                continue
            #avoid writing multiple hits for the same couple (query, subject)
            if multiAlign:
                if debug:
                    print('a hit line was previously printed for query %s and hit sequence %s'%(query, subject))
                continue
            flds = ln.split(' bits ') #left parts is score right part is E-value
            score = flds[0][8:] #remove the 'Score =' part...
            score = float(score.strip())
            hitDict['Score'] = score
            #let's now extract the e-value
            evalue = flds[-1].split('Expect = ')
            evalue = evalue[-1].strip()
            hitDict['E-value'] = evalue
            if debug:
                print('SCORE:\t%s'%str(score))
                print('E-VALUE:\t%s'%evalue)
            #PREPARE THE FINAL LINE
            tmpList = []
            for k in hitDict:
                tmpList.append(str(hitDict[k]))
                if k == 'E-value':
                    break
            #PRINT THE LINE UNTIL THE E-VALUE
            topHitFd.write('%s\n'%'\t'.join(tmpList))
            multiAlign = True #avoid writing multiple hits for the same couple (query, subject)
            tmpList = None
    ### CLOSE THE DATABASES ###
    gidbHandle.close()
    nodesdbHandle.close()
    namesdbHandle.close()
    ranksdbHandle.close()
    #remove local database
    #shutil.rmtree(localDbDir, ignore_errors=True)
    #close input and output files
    fd.close()
    noHitFd.close()
    topHitFd.close()
    missHitFd.close()
    #OUTPUT SUMMARY
    summaryFd = open(summaryPath, 'w')
    summaryFd.write('\nINPUT:%s\n'%inBlastOut)
    summaryFd.write('OUTPUT DIRECTORY:%s\n'%outDir)
    summaryFd.write('OUTPUT LOG FILE:\t%s\n'%outLog)
    summaryFd.write('MAX HITS PER QUERY:%s\n'%str(bestHitsLimit))
    summaryFd.write('TOP HIT OUTPUT FILE:%s\n'%topHitPath)
    summaryFd.write('NO HIT OUTPUT FILE:%s\n'%noHitPath)
    summaryFd.write('NOT PRESENT IN DATABASE OUTPUT FILE:%s\n'%missingHitPath)
    summaryFd.write('SUMMARY FILE:%s\n'%summaryPath)
    summaryFd.write('\nTOT HITS:\t%s\n'%str(totHitCnt))
    summaryFd.write('FOUND IN DB:\t%s\n'%str(hitCnt))
    summaryFd.write('NOT IN DB:\t%s\n'%str(missingHitCnt))
    summaryFd.write('NO HITS FOUND:\t%s\n'%str(noHitCnt))
    summaryFd.close()
    #return output files paths
    return(topHitPath, noHitPath, missingHitPath, summaryPath)



def extractBlastnBestHitsLevDB(inBlastOut, outDir=os.getcwd(), bestHitsLimit=2, outPrefix=None, outLog=False, debug=False):
    '''
    This script will take as input a text file generated using blastn (from BLAST+), and extract the best hits for
    each pair query-subject DNA sequence.
    It relies on a lelevldb database containing taxonomic information
    '''
    limitExceeded = False #used to decide if the hit should be counted or not
    multiAlign = False #used to understand if we are reading a secondary alignment for the same pair (query, subject)
    #write some debug about the input and the program
    if debug:
        sys.stdout.write('\nINPUT:%s\n'%inBlastOut)
        sys.stdout.write('OUTPUT DIRECTORY:%s\n'%outDir)
        sys.stdout.write('MAX HITS PER QUERY:%s\n'%str(bestHitsLimit))
        sys.stdout.write('OUTPUT LOG FILE:\t%s\n'%outLog)
    #check that input file and output directory exist
    if not os.path.isfile(inBlastOut):
        sys.stderr.write('The file %s was not found, please provide the path to a valid blastn output file'%inBlastOut)
        sys.exit(-2)
    #create the output directory
    import sys_tools as systools
    systools.makedir(outDir)
    import leveldb
    #set main variables
    hitCnt = 0
    totHitCnt = 0
    missingHitCnt = 0
    noHitCnt = 0
    dbdate = '20140818' #ncbi taxonomy database version
    #path where the leveldb database is located
    dbdir = '/gpfs1/db/taxonomy/%s/'%dbdate
    #set the paths to the databases
    gidb = '%sgi_taxid_nucl.leveldb'%dbdir
    nodesdb = '%snodes.taxid_parent-taxid.leveldb'%dbdir
    namesdb = '%snames.sci.dmp.leveldb'%dbdir
    ranksdb = '%snodes.taxid_rank.leveldb'%dbdir
    maxHitDict = {} #will contain a counter for each hits
    ### OPEN THE DATABASES ###
    gidbHandle = leveldb.LevelDB(gidb)
    nodesdbHandle = leveldb.LevelDB(nodesdb)
    namesdbHandle = leveldb.LevelDB(namesdb)
    ranksdbHandle = leveldb.LevelDB(ranksdb)
    #########################
    #define output file paths
    topHitPath = noHitPath = missingHitPath = summaryPath = None
    if outPrefix != None:
        topHitPath = '%s%s.tophit.list.txt'%(outDir, outPrefix)
        noHitPath = '%s%s.nohit-id.list.txt'%(outDir, outPrefix)
        missingHitPath = '%s%s.not_in_db-id.list.txt'%(outDir, outPrefix)
        summaryPath = '%s%s.hit.stats.txt'%(outDir, outPrefix)
    else:
        topHitPath = '%s%s.tophit.list.txt'%(outDir, os.path.basename(inBlastOut))
        noHitPath = '%s%s.nohit-id.list.txt'%(outDir, os.path.basename(inBlastOut))
        missingHitPath = '%s%s.not_in_db-id.list.txt'%(outDir, os.path.basename(inBlastOut))
        summaryPath = '%s%s.hit.stats.txt'%(outDir, os.path.basename(inBlastOut))
    if debug:
        sys.stdout.write('\nTOP HIT OUTPUT FILE:%s\n'%topHitPath)
        sys.stdout.write('NO HIT OUTPUT FILE:%s\n'%noHitPath)
        sys.stdout.write('NOT PRESENT IN DATABASE OUTPUT FILE:%s\n'%missingHitPath)
        sys.stdout.write('SUMMARY FILE:%s\n'%summaryPath)
    #start parsing the blasn output file and open the 2 output files
    noHitFd = open(noHitPath, 'w')
    topHitFd = open(topHitPath, 'w')
    missHitFd = open(missingHitPath, 'w')
    query = subject = None #will contain the query id
    #write hdr in the top hits file
    topHitFd.write('#QueryID\tHit_gi\tTaxID\tSuperkingdom\tPhylum\tClass\tOrder\tFamily\tGenus\tSpecies\tScore\tE-value\n')
    fd = open(inBlastOut)
    loopCnt = 0
    loopLimit = 400
    for ln in fd:
        loopCnt +=1
        ln = ln.rstrip()
        if ln == '': continue
        elif ln[:7] == 'Query= ': #make sure that the extra space is present also in older blastn versions
            query = ln[7:]
            if debug:
                print('line:\t%s\n'%ln)
        elif 'No hits found' in ln:
            noHitCnt +=1
            noHitFd.write('%s\n'%query)
            totHitCnt +=1
            if debug:
                print('line:\t%s\n'%ln)
        elif ln[0] == '>': #it is a it so we should write it in the top hits list
            if debug:
                print('line:\t%s\n'%ln)
            subject = ln[1:] #store the hit line
            multiAlign = False #reset the variable since we just found this hit sequence
            #add the query to the dictionary with the count and increment the counter
            if not query in maxHitDict:
                limitExceeded = False
                maxHitDict[query] = 1
            else: #just increment the counter
                if maxHitDict[query] + 1 > bestHitsLimit:
                    limitExceeded = True
                    continue
                else: maxHitDict[query] += 1
            #this is dictinary will contain the final string
            hitDict = OrderedDict()
            hitDict['#QueryID'] = query
            hitCnt +=1
            totHitCnt +=1
            #extract gi parts
            flds = ln.split('|')
            gi = flds[1].strip()
            if debug:
                print('gi:\t%s'%gi)
            taxid = None
            #check that gi is in the gidb database (handle the key-error exception)
            try:
                taxid = gidbHandle.Get(gi)
                hitDict['Hit_gi'] = gi
                hitDict['TaxID'] = taxid
                if debug:
                    print('gi:\t%s'%gi)
                    print('taxid:\t%s'%taxid)
                #os.makedirs(path)
            except KeyError:
                if debug:
                    print('WARNING: %s is not a valid taxid'%gi)
                if taxid == None:
                    raise
            #add the other fields to the dictionary as None
            hitDict['Superkingdom'] = hitDict['Phylum'] = hitDict['Class'] = hitDict['Order'] = hitDict['Family'] = hitDict['Genus'] = hitDict['Species'] = hitDict['Score'] = hitDict['E-value'] = None
            #get the node in the taxonomsy tree
            taxnode = rank = None
            try:
                taxnode = nodesdbHandle.Get(taxid)
                if debug:
                    print('taxnode:\t%s'%nodesdbHandle.Get(taxid))
                #os.makedirs(path)
            except KeyError:
                if debug:
                    print('WARNING: %s is not a valid taxonomy node'%taxid)
                if taxid == None:
                    raise
            #let's now loop to find all the taxonomic levels
            #if nodesdbHandle.Get(taxid) != None:
            if taxnode != None:
                #set the values in the dictionary
                while nodesdbHandle.Get(taxid) != taxid:
                    hitDict[ranksdbHandle.Get(taxid).title()] = namesdbHandle.Get(taxid)
                    #update the values to update the while's condition outcome
                    taxid = nodesdbHandle.Get(taxid)
            else:#write the line in the file with not found
                missHitFd.write('%s\n'%ln)
                if debug:
                    print(ln)
                missingHitCnt +=1
                hitCnt -=1
                #sys.exit('debug')
                continue
            #CHECK THE DICTIONARY CONTENT
            for k in hitDict:
                if debug:
                    print('%s::\t%s'%(k, hitDict[k]))
        elif ln[:8] == ' Score =': #this line contain sthe score and e value
            if debug:
                print('Score line:\t%s\n'%ln)
            #if too many lines for the query has been printed, than skipt it
            if limitExceeded:
                if debug:
                    print('%s hits had been printed for query %s, while the max limit is %s'%(str(maxHitDict[query]), query, str(bestHitsLimit)))
                continue
            #avoid writing multiple hits for the same couple (query, subject)
            if multiAlign:
                if debug:
                    print('a hit line was previously printed for query %s and hit sequence %s'%(query, subject))
                continue
            flds = ln.split(' bits ') #left parts is score right part is E-value
            score = flds[0][8:] #remove the 'Score =' part...
            score = float(score.strip())
            hitDict['Score'] = score
            #let's now extract the e-value
            evalue = flds[-1].split('Expect = ')
            evalue = evalue[-1].strip()
            hitDict['E-value'] = evalue
            if debug:
                print('SCORE:\t%s'%str(score))
                print('E-VALUE:\t%s'%evalue)
            #PREPARE THE FINAL LINE
            tmpList = []
            for k in hitDict:
                tmpList.append(str(hitDict[k]))
                if k == 'E-value':
                    break
            #PRINT THE LINE UNTIL THE E-VALUE
            topHitFd.write('%s\n'%'\t'.join(tmpList))
            multiAlign = True #avoid writing multiple hits for the same couple (query, subject)
            tmpList = None
    ### CLOSE THE DATABASES ###
    del gidbHandle
    del nodesdbHandle
    del namesdbHandle
    del ranksdbHandle
    #close input and output files
    fd.close()
    noHitFd.close()
    topHitFd.close()
    missHitFd.close()
    #OUTPUT SUMMARY
    summaryFd = open(summaryPath, 'w')
    summaryFd.write('\nINPUT:%s\n'%inBlastOut)
    summaryFd.write('OUTPUT DIRECTORY:%s\n'%outDir)
    summaryFd.write('OUTPUT LOG FILE:\t%s\n'%outLog)
    summaryFd.write('MAX HITS PER QUERY:%s\n'%str(bestHitsLimit))
    summaryFd.write('TOP HIT OUTPUT FILE:%s\n'%topHitPath)
    summaryFd.write('NO HIT OUTPUT FILE:%s\n'%noHitPath)
    summaryFd.write('NOT PRESENT IN DATABASE OUTPUT FILE:%s\n'%missingHitPath)
    summaryFd.write('SUMMARY FILE:%s\n'%summaryPath)
    summaryFd.write('\nTOT HITS:\t%s\n'%str(totHitCnt))
    summaryFd.write('FOUND IN DB:\t%s\n'%str(hitCnt))
    summaryFd.write('NOT IN DB:\t%s\n'%str(missingHitCnt))
    summaryFd.write('NO HITS FOUND:\t%s\n'%str(noHitCnt))
    summaryFd.close()
    #return output files paths
    return(topHitPath, noHitPath, missingHitPath, summaryPath)



def extractBlastnBestHitsVirusFromTable(inBlastOut, outDir=os.getcwd(), bestHitsLimit=1, outPrefix=None, outLog=False, debug=False):
    '''
    This script will take as input a text file generated using blastn (from BLAST+), and extract the best hits for
    each pair query-subject DNA sequence.
    It relies on a kyotocabinet databases containing taxonomic information
    '''
    #refTableVir = givir2lineage
    refTableVir = '%svirus_gi2lineage.txt'%getResourceRoot()
    limitExceeded = False #used to decide if the hit should be counted or not
    multiAlign = False #used to understand if we are reading a secondary alignment for the same pair (query, subject)
    #write some debug about the input and the program
    if debug:
        sys.stdout.write('\nINPUT:%s\n'%inBlastOut)
        sys.stdout.write('OUTPUT DIRECTORY:%s\n'%outDir)
        sys.stdout.write('MAX HITS PER QUERY:%s\n'%str(bestHitsLimit))
        sys.stdout.write('OUTPUT LOG FILE:\t%s\n'%outLog)
        sys.stdout.write('Reference table with virus lineage:\n%s\n'%refTableVir)
    #check that input file and output directory exist
    if not os.path.isfile(inBlastOut):
        sys.stderr.write('The file %s was not found, please provide the path to a valid blastn output file'%inBlastOut)
        sys.exit(-2)
    #create the output directory
    import sys_tools as systools
    systools.makedir(outDir)
    #set main variables
    hitCnt = 0
    totHitCnt = 0
    missingHitCnt = 0
    noHitCnt = 0
    maxHitDict = {} #will contain a counter for each hits
    ### LOAD THE LINEAGE TABLE ###
    lineageDict = OrderedDict()
    ifd = open(refTableVir)
    for ln in ifd:
        if ln.startswith('gi'):
            continue
        ln = ln.rstrip('\n')
        tmpGi, lineage = ln.split('\t', 1)
        lineageDict[tmpGi] = lineage
    ifd.close()
    if debug:
        print('%d elements loaded in dictionary\n'%len(lineageDict))
        print('System size for the dictionary:\t%d\n'%sys.getsizeof(lineageDict))
    ##############################
    #define output file paths
    topHitPath = noHitPath = missingHitPath = summaryPath = None
    if outPrefix != None:
        topHitPath = '%s%s.tophit.virus.list.txt'%(outDir, outPrefix)
        noHitPath = '%s%s.nohit-id.virus.list.txt'%(outDir, outPrefix)
        missingHitPath = '%s%s.not_in_db-id.virus.list.txt'%(outDir, outPrefix)
        summaryPath = '%s%s.hit.virus.stats.txt'%(outDir, outPrefix)
    else:
        topHitPath = '%s%s.tophit.virus.list.txt'%(outDir, os.path.basename(inBlastOut))
        noHitPath = '%s%s.nohit-id.virus.list.txt'%(outDir, os.path.basename(inBlastOut))
        missingHitPath = '%s%s.not_in_db-id.virus.list.txt'%(outDir, os.path.basename(inBlastOut))
        summaryPath = '%s%s.hit.virus.stats.txt'%(outDir, os.path.basename(inBlastOut))
    if debug:
        sys.stdout.write('\nTOP HIT OUTPUT FILE:%s\n'%topHitPath)
        sys.stdout.write('NO HIT OUTPUT FILE:%s\n'%noHitPath)
        sys.stdout.write('NOT PRESENT IN DATABASE OUTPUT FILE:%s\n'%missingHitPath)
        sys.stdout.write('SUMMARY FILE:%s\n'%summaryPath)
    #start parsing the blasn output file and open the 2 output files
    noHitFd = open(noHitPath, 'w')
    topHitFd = open(topHitPath, 'w')
    missHitFd = open(missingHitPath, 'w')
    query = subject = None #will contain the query id
    #write hdr in the top hits file
    topHitFd.write('QueryID\tHit_gi\tTaxID\tDivision\tSuperkingdom\tPhylum\tClass\tOrder\tFamily\tGenus\tSpecies\tScore\tE-value\n')
    #read the input file
    fd = open(inBlastOut)
    loopCnt = 0
    loopLimit = 400
    for ln in fd:
        loopCnt +=1
        ln = ln.rstrip()
        if ln == '': continue
        elif ln[:7] == 'Query= ': #make sure that the extra space is present also in older blastn versions
            query = ln[7:]
            if debug:
                print('line:\t%s\n'%ln)
        elif 'No hits found' in ln:
            noHitCnt +=1
            noHitFd.write('%s\n'%query)
            totHitCnt +=1
            if debug:
                print('line:\t%s\n'%ln)
        elif ln[0] == '>': #it is a it so we should write it in the top hits list
            if debug:
                print('line:\t%s\n'%ln)
            subject = ln[1:] #store the hit line
            multiAlign = False #reset the variable since we just found this hit sequence
            #add the query to the dictionary with the count and increment the counter
            if not query in maxHitDict:
                limitExceeded = False
                maxHitDict[query] = 1
            else: #just increment the counter
                if maxHitDict[query] + 1 > bestHitsLimit:
                    limitExceeded = True
                    continue
                else: maxHitDict[query] += 1
            #this dictionary will contain the final string
            hitDict = OrderedDict()
            hitDict['QueryID'] = query
            hitCnt +=1
            totHitCnt +=1
            #extract gi parts
            flds = ln.split('|')
            gi = flds[1].strip()
            if debug:
                print('gi:\t%s'%gi)
            hitDict['Hit_gi'] = gi
            taxid = None
            #get the lineage for the gi
            lng, found = getNcbiLineageVirusFromTable(gi, taxDict=lineageDict, debug=debug)
            foundGi, taxid, lineage = lng.split('\t', 2)
            hitDict['TaxID'] = taxid
            #check that gi is in the gidb database (handle the key-error exception)
            if (found == False) or (taxid == 'None'):
                if debug:
                    print('WARNING: %s was not found in the lineage table for viruses'%gi)
                    print(lng)
                missHitFd.write('%s\n'%lng)
                missingHitCnt +=1
                hitCnt -=1
                #add the other fields to the dictionary as None
                hitDict['Division'] = hitDict['Superkingdom'] = hitDict['Phylum'] = hitDict['Class'] = hitDict['Order'] = hitDict['Family'] = hitDict['Genus'] = hitDict['Species'] = hitDict['Score'] = hitDict['E-value'] = None
            else:
                division, superk, ph, cl, order, family, genus, species, dummy = lineage.split('\t', 8)
                hitDict['Division'] = division
                hitDict['Superkingdom'] = superk
                hitDict['Phylum'] = ph
                hitDict['Class'] = cl
                hitDict['Order'] = order
                hitDict['Family'] = family
                hitDict['Genus'] = genus
                hitDict['Species'] = species
        elif ln[:8] == ' Score =': #this line contain sthe score and e value
            if debug:
                print('Score line:\t%s\n'%ln)
            #if too many lines for the query has been printed, than skipt it
            if limitExceeded:
                if debug:
                    print('%s hits had been printed for query %s, while the max limit is %s'%(str(maxHitDict[query]), query, str(bestHitsLimit)))
                continue
            #avoid writing multiple hits for the same couple (query, subject)
            if multiAlign:
                if debug:
                    print('a hit line was previously printed for query %s and hit sequence %s'%(query, subject))
                continue
            flds = ln.split(' bits ') #left parts is score right part is E-value
            score = flds[0][8:] #remove the 'Score =' part...
            score = float(score.strip())
            hitDict['Score'] = score
            #let's now extract the e-value
            evalue = flds[-1].split('Expect = ')
            evalue = evalue[-1].strip()
            hitDict['E-value'] = evalue
            if debug:
                print('Score:\t%s'%str(score))
                print('E-value:\t%s'%evalue)
            #PREPARE THE FINAL LINE
            tmpList = []
            for k in hitDict:
                tmpList.append(str(hitDict[k]))
                if k == 'E-value':
                    break
            #PRINT THE LINE UNTIL THE E-VALUE
            hitLine = '\t'.join(tmpList)
            topHitFd.write('%s\n'%hitLine)
            multiAlign = True #avoid writing multiple hits for the same couple (query, subject)
            tmpList = None
    #close input and output files
    fd.close()
    noHitFd.close()
    topHitFd.close()
    missHitFd.close()
    #OUTPUT SUMMARY
    summaryFd = open(summaryPath, 'w')
    summaryFd.write('\nINPUT:%s\n'%inBlastOut)
    summaryFd.write('OUTPUT DIRECTORY:%s\n'%outDir)
    summaryFd.write('OUTPUT LOG FILE:\t%s\n'%outLog)
    summaryFd.write('MAX HITS PER QUERY:%s\n'%str(bestHitsLimit))
    summaryFd.write('TOP HIT OUTPUT FILE:%s\n'%topHitPath)
    summaryFd.write('NO HIT OUTPUT FILE:%s\n'%noHitPath)
    summaryFd.write('NOT PRESENT IN DATABASE OUTPUT FILE:%s\n'%missingHitPath)
    summaryFd.write('SUMMARY FILE:%s\n'%summaryPath)
    summaryFd.write('\nTOT HITS:\t%s\n'%str(totHitCnt))
    summaryFd.write('FOUND IN DB:\t%s\n'%str(hitCnt))
    summaryFd.write('NOT IN DB:\t%s\n'%str(missingHitCnt))
    summaryFd.write('NO HITS FOUND:\t%s\n'%str(noHitCnt))
    summaryFd.close()
    #return output files paths
    return(topHitPath, noHitPath, missingHitPath, summaryPath)



def getBlastDbs():
    """Returns the dictionary with blast databases."""
    return(blastDbPaths)



def getBinRoot():
    """returns the to the to the binary programs."""
    return bin_root



def getBlastnDbsRoot():
    """returns the path to the root blastn dbs."""
    return blastn_dbs_root



def getBlastpDbsRoot():
    """returns the path to the root blastp dbs."""
    return blastp_dbs_root



def getResourceRoot():
    """returns the to the resource directory."""
    return resource_root



def getRoot():
    """returns the to the root directory."""
    return root



def setBinRoot(path):
    """set the path to of the executables directory."""
    global bin_root
    if os.path.isdir(path):
        bin_root = path
        if bin_root[-1] != '/':
            bin_root += '/'
    else:
        sys.stderr.write('ERROR: %s is not a valid directory, please check metapatfinder config file'%path)
        sys.exit(-2)
    return bin_root



def setBlastnDbsRoot(path):
    """set the path to of the blastn database directory."""
    global blastn_dbs_root
    if os.path.isdir(path):
        blastn_dbs_root = path
        if blastn_dbs_root[-1] != '/':
            blastn_dbs_root += '/'
    else:
        sys.stderr.write('ERROR: %s is not a valid directory, please check metapatfinder config file'%path)
        sys.exit(-2)
    return blastn_dbs_root



def setBlastpDbsRoot(path):
    """set the path to of the blastp database directory."""
    global blastp_dbs_root
    if os.path.isdir(path):
        blastp_dbs_root = path
        if blastp_dbs_root[-1] != '/':
            blastp_dbs_root += '/'
    else:
        sys.stderr.write('ERROR: %s is not a valid directory, please check metapatfinder config file'%path)
        sys.exit(-2)
    return blastp_dbs_root



def setResourceRoot(path):
    """set the path to of the resource directory."""
    global resource_root
    if os.path.isdir(path):
        resource_root = path
        if resource_root[-1] != '/':
            resource_root += '/'
    else:
        sys.stderr.write('ERROR: %s is not a valid directory, please check metapatfinder config file'%path)
        sys.exit(-2)
    return resource_root



def setRoot(path):
    """set the path to of the root directory."""
    global root
    if os.path.isdir(path):
        root = path
        if root[-1] != '/':
            root += '/'
        #update all the affected paths
        updatePaths(root)
    else:
        sys.stderr.write('ERROR: %s is not a valid directory, please check metapatfinder config file'%path)
        sys.exit(-2)
    return root



def updatePaths(rootPath):
    global bin_root
    global blastn_dbs_root
    global blastp_dbs_root
    global resource_root
    #executables directory
    bin_root = '%sbin/'%rootPath
    if not os.path.isdir(bin_root):
        sys.stderr.write('ERROR: %s is not a valid directory, please check metapatfinder config file.\n'%bin_root)
        sys.exit(-2)
    #blastn databases
    blastn_dbs_root = '%sdbs/blastn/'%rootPath
    if not os.path.isdir(blastn_dbs_root):
        sys.stderr.write('ERROR: %s is not a valid directory, please check metapatfinder config file.\n'%blastn_dbs_root)
        sys.exit(-2)
    #blastp databases
    blastp_dbs_root = '%sdbs/blastp/'%rootPath
    if not os.path.isdir(blastp_dbs_root):
        sys.stderr.write('ERROR: %s is not a valid directory, please check metapatfinder config file.\n'%blastp_dbs_root)
        sys.exit(-2)
    #resources directory
    resource_root = '%sresources/'%rootPath
    if not os.path.isdir(resource_root):
        sys.stderr.write('ERROR: %s is not a valid directory, please check metapatfinder config file.\n'%resource_root)
        sys.exit(-2)



def updateBlastDbPaths(path):
    '''update the dictionary with the blast db paths'''
    global blastDbPaths
    blastDbPaths = {'ncbi_viruses_ntv':'%sntv_20150615'%blastn_dbs_root}



def updateBlastToolsPaths(path, debug=False):
    '''update the binary paths to the tools.'''
    global blastTools
    if debug:
        print('\nupdateBlastToolsPaths START:')
        print('New bin directory:\n%s'%path)
    for program in blastTools:
        blastTools[program] = '%s%s'%(path, program)
        if not os.path.isfile(blastTools[program]):
            sys.stderr.write('ERROR: the path for %s program is not valid\n%s\nplease check metapatfinder config file.\n'%(program, blastTools[program]))
            sys.exit(-2)
    if debug:
        print('Paths to the blast programs correctly updated.')



def getNcbiLineageKc(giId, debug=False):
    '''Extract ncbi taxonomy lineage for an in gi id'''
    import shutil
    import kyotocabinet as kc
    if debug:
        sys.stdout.write('\nINPUT:%s\n'%giId)
    #set main variables
    dbdate = '20141215' #ncbi taxonomy database version
    #path where the leveldb database is located
    dbdir = '/user/gen-info/salvocos/projects/dbs/local/ncbi_taxonomy/kyotocabinet/%s/'%dbdate
    #set the paths to the databases
    dbExt = 'kct'
    gidb = '%sgi_taxid_nucl.%s'%(dbdir, dbExt)
    dbExt = 'kch'
    #Database path
    nodesdb = '%snodes_taxid_parent-taxid.%s'%(dbdir, dbExt)
    namesdb = '%snames_sci.%s'%(dbdir, dbExt)
    ranksdb = '%snodes_taxid2rank.%s'%(dbdir, dbExt)
    ### OPEN THE DATABASES ###
    gidbHandle = kc.DB()
    gidbHandle.open(gidb, kc.DB.OREADER | kc.DB.ONOLOCK)
    nodesdbHandle = kc.DB()
    nodesdbHandle.open(nodesdb, kc.DB.OREADER | kc.DB.ONOLOCK)
    namesdbHandle = kc.DB()
    namesdbHandle.open(namesdb, kc.DB.OREADER | kc.DB.ONOLOCK)
    ranksdbHandle = kc.DB()
    ranksdbHandle.open(ranksdb, kc.DB.OREADER | kc.DB.ONOLOCK)
    if debug: #print db paths
        print('gidbHandle :: %s'%gidb)
        print('nodesdbHandle :: %s'%nodesdb)
        print('namesdbHandle :: %s'%namesdb)
        print('ranksdbHandle :: %s'%ranksdb)
    #########################
    #write hdr in the top hits file
    #topHitFd.write('#QueryID\tHit_gi\tTaxID\tSuperkingdom\tPhylum\tClass\tOrder\tFamily\tGenus\tSpecies\tScore\tE-value\n')
    #gi = flds[1].strip()
    if debug:
        print('gi:\t%s'%giId)
    taxid = None
    hitDict = OrderedDict()
    #check that gi is in the gidb database (handle the key-error exception)
    try:
        taxid = gidbHandle.get(giId)
        hitDict['gi'] = giId
        hitDict['taxid'] = taxid
        if debug:
            print('gi:\t%s'%giId)
            print('taxid:\t%s'%taxid)
    except KeyError:
        if debug:
            print('WARNING: %s is not a valid taxid'%giId)
        if taxid == None:
            raise
    #add the other fields to the dictionary as None
    hitDict['Superkingdom'] = hitDict['Phylum'] = hitDict['Class'] = hitDict['Order'] = hitDict['Family'] = hitDict['Genus'] = hitDict['Species'] = None
    #get the node in the taxonomy tree
    taxnode = rank = None
    try:
        #taxnode = None
        taxnode = nodesdbHandle.get(taxid)
        if debug:
            print('taxnode:\t%s'%taxnode)
    except KeyError:
        if debug:
            print('WARNING: %s is not a valid taxonomy node'%taxid)
        if taxid == None:
            raise
    #let's now loop to find all the taxonomic levels
    if taxnode != None:
        #set the values in the dictionary
        while nodesdbHandle.get(taxid) != taxid:
            hitDict[ranksdbHandle.get(taxid).title()] = namesdbHandle.get(taxid)
            #update the values to update the while's condition outcome
            taxid = nodesdbHandle.get(taxid)
            #sys.exit('DEBUG: taxnode!= None')
    else:#write the line in the file with not found
        sys.stderr.write('\nWARNING: no taxonomy found for %s'%giId)
        return None
        #sys.exit(-3)
    #CHECK THE DICTIONARY CONTENT
    if debug:
        print('\nCHECK THE DICTIONARY CONTENT:')
        for k in hitDict:
            print('%s::\t%s'%(k, hitDict[k]))
    if debug:
        print('\t'.join(hitDict.keys()))
        print('\t'.join(map(str, hitDict.values())))
    ### CLOSE THE DATABASES ###
    gidbHandle.close()
    nodesdbHandle.close()
    namesdbHandle.close()
    ranksdbHandle.close()
    #return the dictionary containing the taxonomy levels as keys
    #and taxonomy names as values
    return hitDict



def getNcbiLineageKcVirus(giId, debug=False):
    '''Extract ncbi taxonomy lineage for an in gi id'''
    import shutil
    import kyotocabinet as kc
    if debug:
        sys.stdout.write('\nINPUT:%s\n'%giId)
    #set main variables
    dbdate = '20150911_virus' #ncbi taxonomy database version
    #path where the leveldb database is located
    dbdir = '/user/gen-info/salvocos/projects/dbs/local/ncbi_taxonomy/kyotocabinet/%s/'%dbdate
    #set the paths to the databases
    dbExt = 'kct'
    gidb = '%sgi_taxid_nucl_v.%s'%(dbdir, dbExt)
    #dbExt = 'kch'
    #Database path
    nodesdb = '%snodes_taxid2parent-taxid.%s'%(dbdir, dbExt)
    namesdb = '%snames_sci.%s'%(dbdir, dbExt)
    ranksdb = '%snodes_taxid2rank.%s'%(dbdir, dbExt)
    #DIVISION DB
    divdb = '%snodes_taxid2division.%s'%(dbdir, dbExt)
    #THE DICTIONARY WILL MAP THE DIVISION NAME
    '''
    0	|	BCT	|	Bacteria	|		|
    1	|	INV	|	Invertebrates	|		|
    2	|	MAM	|	Mammals	|		|
    3	|	PHG	|	Phages	|		|
    4	|	PLN	|	Plants	|		|
    5	|	PRI	|	Primates	|		|
    6	|	ROD	|	Rodents	|		|
    7	|	SYN	|	Synthetic	|		|
    8	|	UNA	|	Unassigned	|	No species nodes should inherit this division assignment	|
    9	|	VRL	|	Viruses	|		|
    10	|	VRT	|	Vertebrates	|		|
    11	|	ENV	|	Environmental samples	|	Anonymous sequences cloned directly from the environment	|
    '''
    divisionDict = OrderedDict([('0', 'Bacteria'), ('1', 'Invertebrates'), ('2', 'Mammals'), ('3', 'Phages'), ('4', 'Plants'), ('5', 'Primates'), ('6', 'Rodents'), ('7', 'Synthetic'), ('8', 'Unassigned'), ('9', 'Viruses'), ('10', 'Vertebrates'), ('11', 'Environmental samples')])
    ### OPEN THE DATABASES ###
    gidbHandle = kc.DB()
    gidbHandle.open(gidb, kc.DB.OREADER | kc.DB.ONOLOCK)
    nodesdbHandle = kc.DB()
    nodesdbHandle.open(nodesdb, kc.DB.OREADER | kc.DB.ONOLOCK)
    namesdbHandle = kc.DB()
    namesdbHandle.open(namesdb, kc.DB.OREADER | kc.DB.ONOLOCK)
    ranksdbHandle = kc.DB()
    ranksdbHandle.open(ranksdb, kc.DB.OREADER | kc.DB.ONOLOCK)
    divdbHandle = kc.DB()
    divdbHandle.open(divdb, kc.DB.OREADER | kc.DB.ONOLOCK)
    if debug: #print db paths
        print('gidbHandle :: %s'%gidb)
        print('nodesdbHandle :: %s'%nodesdb)
        print('namesdbHandle :: %s'%namesdb)
        print('ranksdbHandle :: %s'%ranksdb)
        print('divdbHandle :: %s'%divdb)
    #sys.exit('debug')
    #########################
    #write hdr in the top hits file
    #topHitFd.write('#QueryID\tHit_gi\tTaxID\tSuperkingdom\tPhylum\tClass\tOrder\tFamily\tGenus\tSpecies\tScore\tE-value\n')
    #gi = flds[1].strip()
    if debug:
        print('gi:\t%s'%giId)
    taxid = None
    hitDict = OrderedDict()
    #check that gi is in the gidb database (handle the key-error exception)
    try:
        taxid = gidbHandle.get(giId)
        hitDict['gi'] = giId
        hitDict['taxid'] = taxid
        if debug:
            print('gi:\t%s'%giId)
            print('taxid:\t%s'%taxid)
    except KeyError:
        if debug:
            print('WARNING: %s is not a valid taxid'%giId)
        if taxid == None:
            raise
    #add the other fields to the dictionary as None
    hitDict['Division'] = hitDict['Superkingdom'] = hitDict['Phylum'] = hitDict['Class'] = hitDict['Order'] = hitDict['Family'] = hitDict['Genus'] = hitDict['Species'] = None
    #get the node in the taxonomy tree
    taxnode = rank = None
    try:
        #taxnode = None
        taxnode = nodesdbHandle.get(taxid)
        if debug:
            print('taxnode:\t%s'%taxnode)
    except KeyError:
        if debug:
            print('WARNING: %s is not a valid taxonomy node'%taxid)
        if taxid == None:
            raise
    #let's now loop to find all the taxonomic levels
    division = None
    if taxnode != None:
        #set the values in the dictionary
        while nodesdbHandle.get(taxid) != taxid:
            #if debug:
                #print('\n#')
                #print(taxnode)
                #print(taxid)
            if taxid == '1':
                taxid = nodesdbHandle.get(taxid)
                continue
            #extract the diviosion
            if division is None:
                division = divdbHandle.get(taxid)
                if division in divisionDict:
                    division = divisionDict[division]
                else:
                    division = None
                hitDict['Division'] = division
                print(hitDict['Division'])
            hitDict[ranksdbHandle.get(taxid).title()] = namesdbHandle.get(taxid)
            #update the values to update the while's condition outcome
            taxid = nodesdbHandle.get(taxid)
            #sys.exit('DEBUG: taxnode!= None')
    else:#write the line in the file with not found
        sys.stderr.write('\nWARNING: no taxonomy found for %s'%giId)
        return(hitDict, False)
        #sys.exit(-3)
    #CHECK THE DICTIONARY CONTENT
    if debug:
        print('\nCHECK THE DICTIONARY CONTENT:')
        for k in hitDict:
            print('%s::\t%s'%(k, hitDict[k]))
    if debug:
        print('\t'.join(hitDict.keys()))
        print('\t'.join(map(str, hitDict.values())))
    ### CLOSE THE DATABASES ###
    gidbHandle.close()
    nodesdbHandle.close()
    namesdbHandle.close()
    ranksdbHandle.close()
    divdbHandle.close()
    #return the dictionary containing the taxonomy levels as keys
    #and taxonomy names as values
    return(hitDict, True)



def getNcbiLineageVirusFromTable(gi, taxDict=None, debug=False):
    """get the ncbi viral lineage using a tab separated table."""
    if debug:
        print('\ngetNcbiLineageVirusFromTable START:')
        print('Genbank id:\t%s'%gi)
    #refTable = givir2lineage
    refTableVir = '%svirus_gi2lineage.txt'%getResourceRoot()
    #load the table in a dictionary
    lineageDict = OrderedDict()
    #load the dictionary if needed
    if taxDict is None:
        #open and read the file
        ifd = open(refTable)
        for ln in ifd:
            if ln.startswith('gi'):
                continue
            ln = ln.rstrip('\n')
            tmpGi, lineage = ln.split('\t', 1)
            lineageDict[tmpGi] = lineage
        ifd.close()
    else:
        if debug:
            print('Using external dictionary')
        lineageDict = taxDict
    if debug:
        print('%d elements loaded in dictionary\n'%len(lineageDict))
        print('System size for the dictionary:\t%d\n'%sys.getsizeof(lineageDict))
    noneLn = '%s\tNone\tNone\tNone\tNone\tNone\tNone\tNone\tNone\tNone'%gi
    #check if the string is available
    if gi in lineageDict:
        return('%s\t%s'%(gi, lineageDict[gi]), True)
    else:
        return(noneLn, False)



def makedb(inSeq, outDir=os.getcwd(), dbName=None, dbType='nucl', debug=False):
    '''Create a blast database from an input fasta sequence.'''
    #check that the input file and the database exist
    if not os.path.isfile(inSeq):
        sys.stderr.write('The file %s was not found, please provide the path to a valid FASTA file'%inSeq)
        sys.exit(-2)
    #check if the database exists
    if not os.path.isdir(outDir):
        sys.stderr.write('The directory %s was not found, please provide the path to a valid directory'%outDir)
        sys.exit(-2)
    #check the database type si valid
    dbType = dbType.lower().strip()
    validDbType = ['nucl', 'prot']
    if not dbType in validDbType:
        sys.stderr.write('The chosen format (%s) is not valid, please choose among one of the following:\n%s'%(dbType, str(validDbType)))
        sys.exit(-2)
    #check the db name
    if dbName == None:
        flds = os.path.basename(inSeq)
        flds = flds.split('.')
        dbName = '%s_%s_blastdb'%(flds[0], dbType)
    #create the command to be executed
    #EXAMPLE; makeblastdb -in in.fasta -dbtype nucl -out /outdir/mydb -title mydb
    makeDbCmd = '%s -in %s -dbtype \'%s\' -out %s -title %s'%(blastTools['makeblastdb'], inSeq, dbType, outDir+dbName, dbName)
    if debug:
        print(makeDbCmd)
    #execute the system call
    process = subprocess.Popen(makeDbCmd, shell=True, stdout=subprocess.PIPE, stderr=subprocess.PIPE)
    stdout_val, stderr_val = process.communicate() #get stdout and stderr
    process.wait()
    if debug:
        print('STDOUT:\n%s\n'%stdout_val)
        print('STDERR:\n%s\n'%stderr_val)
    #return a tuple with the results
    #stdout, stderr, outdir, dbname
    return(stdout_val, stderr_val, makeDbCmd, outDir, dbName)



def xmlToBlastFmt7(inXml, outDir=os.getcwd(), debug=False):
    '''
    This function will parse a blast generated xml output file and convert it to a tab separated format
    similar to the standard format 7 from blast
    '''
    from Bio.Blast import NCBIXML
    #define the output file path
    outFile = inXml.replace('.xml', '_mod.txt')
    stdHdr = '#qId\tsbjId\t%identity\talignLen\tmismatches\tgaps\tqStart\tqEnd\tsbjStart\tsbjEnd\tevalue\tbscore\tqCovHsp'
    #open output file
    fdOut = open(outFile, 'w')
    ##### PARSE XML RESULTS #######
    xmlFd = open(inXml)
    blast_records = NCBIXML.parse(xmlFd)
    #E_VALUE_THRESH = 0.04 could be used to filter by evalue
    for blastRec in blast_records:
        qId = str(blastRec.query)
        #print blastRec.alignments
        if len(blastRec.alignments) > 0:
            print(len(blastRec.alignments))
            fdOut.write('%s\n'%stdHdr)
        else:
            fdOut.write('#%s\n'%qId)
        if debug:
            print('query:\t%s'%(qId))
            print('query_letter:\t%s'%(str(blastRec.query_letters)))
            print('len(blastRec.alignments):\t%s'%(str(len(blastRec.alignments))))
        totHits = 0
        for i, alignment in enumerate(blastRec.alignments):
            mTitle = alignment.title #match title
            mId = alignment.accession
            hitId = alignment.hit_id
            hitDef = alignment.hit_def
            if debug:
                print('\n****Alignment****%s'%str(i))
                print('alignment ACC:', mId)
                print('matching_seq:', mTitle)
                print('HIT ID:', hitId)
                print('HIT DEF:', hitDef)
                print('matching_seq_length:', alignment.length)
            hits = len(alignment.hsps)
            totHits = totHits + hits
            for hsp in alignment.hsps:
                #if hsp.expect < E_VALUE_THRESH:
                alignLen = hsp.align_length
                identities = hsp.identities
                alignPerc = round(float(identities)/float(alignLen)*100, 2)
                qLength = blastRec.query_letters
                gaps = hsp.gaps
                mismatches = alignLen-identities-gaps
                #compute sequence start and end position
                qDirection, sbjDirection = hsp.frame # directions needed to calculate end positions
                qStart = hsp.query_start
                qEnd = hsp.query_end
                gapsExtension = float(gaps)/2.
                sbjStart = hsp.sbjct_start
                sbjEnd = hsp.sbjct_end
                evalue = hsp.expect
                bScore = hsp.bits
                qCovHsp = round((float(alignLen)/float(qLength))*100, 2)
                resStr = '%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s\n'%(qId, hitDef, alignPerc, alignLen, mismatches, gaps, qStart, qEnd, sbjStart, sbjEnd, evalue, bScore, qCovHsp)
                fdOut.write(resStr)
                if debug:
                    print('\n**** HSP ****')
                    print('frame:', hsp.frame)
                    print('strands:', hsp.strand)
                    print('query start:\t%s'%(str(hsp.query_start)))
                    print('subject start:\t%s'%(str(hsp.sbjct_start)))
                    print('QEND:\t%s'%(qEnd))
                    print('SBJEND:\t%s'%(sbjEnd))
                    print('score:\t%s'%(str(hsp.score)))
                    print('bits:\t%s'%str(bScore))
                    print('e value:', evalue)
                    print('num_alignments:', hsp.num_alignments)
                    print('identities:', hsp.identities) #alignment length
                    print('positives:', hsp.positives) #positives
                    print('mismatches:', mismatches)
                    print('gaps:', gaps)
                    print('Alignment Perc:\t%s'%(alignPerc))
                    print('HSP Query Coverage Perc (qcovhsp):\t%s'%(qCovHsp))
        if debug:
            print('total HITS for the query:\t%s\n'%str(totHits))
        fdOut.write('#HITS\t%s\n'%str(totHits))
    xmlFd.close()



def rawBlastOutToDB(blastFilePath, outDbPath=None, overwrite=False, chunkSize=10000, syncMode=False, debug=False):
    '''
    This function will parse a blast standard output file
    to extract and store the needed information in a leveldb database
    NOTE: this parser has been tested only with default out (fmt=0) from
    blastn: 2.2.29+
    Package: blast 2.2.29, build Dec 10 2013 14:41:40
    '''
    import leveldb
    import struct #to create a binary buffer to be stored in the database
    #define the output file path
    #stdHdr = '#qId\tsbjId\t%identity\talignLen\tmismatches\tgaps\tqStart\tqEnd\tsbjStart\tsbjEnd\tevalue\tbscore\tqCovHsp'
    #define the output file path
    #set the db path
    if outDbPath == None:
        outDbPath = os.path.basename(blastFilePath)
        flds = outDbPath.split('.')
        if len(flds) > 1:
            outDbPath = flds[0] + '_db'
    outDbPath = os.path.dirname(blastFilePath) + '/' + outDbPath
    #variable to decide if the writing is in batch mode
    batchMode = None
    if chunkSize == 0:
        batchMode = False
    else:
        batchMode = True
    #CREATE THE DATABASE
    if os.path.isdir(outDbPath):
        if debug:
            print('The database %s already exists'%outDbPath)
        if overwrite: #then destroy the database and recreate it
            if debug:
                print('The existing database %s will be deleted '%outDbPath)
            leveldb.DestroyDB(outDbPath)
        else:
            sys.stderr.write('Please remove the existing databse or se the overwrite flag to True')
    if debug:
        print('DB PATH:\n%s'%outDbPath)
        print('CHUNK SIZE:\t%s'%str(chunkSize))
        if chunkSize!=0:
            print('BATCH WRITE MODE ON')
        else:
            print('BATCH WRITE MODE OFF')
        print('SYNC WRITE MODE:\t%s'%str(syncMode))
    mydb = leveldb.LevelDB(outDbPath)
    ####### PARSE THE BLAST OUTPUT FILE ###########
    ctrlDict = {} # it will simply be used to understand if a db id has been repeated
    fd = open(blastFilePath, 'rb')
    qHitOpen = False #used to see if we are in the part contaninig the hits
    readingHitDef = False #used to see if we are extracting the match title
    readingHsp = False #used to see if we are extracting the info and sequences of a given hsp
    qLen = hitLen = 0 #query length
    qId = hId = ''
    bCnt =0
    dbRecCnt = 0
    cnt = 0
    batch = None
    goodHspCnt = 0 #will count the the hits with good bitscore
    hspCnt = 0 #will count the number of hits
    hspParseChk = 0 #will be used to see if all the hsp has been correctly extracted
    refSeqLen = qSeq = sbjSeq = matchSeq = qStart = qEnd = sbjStart = sbjEnd = qDir = sbjDir = identities = idPerc = gaps = alignLen = bScore = evalue = hId = qLen = hitLen = None
    packedRec = dbRecId = None
    for ln in fd:
        if ln[4:6] == 'y=':
            ln = ln[6:-1].strip() #remove the "Query="
            if qHitOpen:
                #### WRITE THE RECORD IN THE DATABASE IF PENDING ####
                if matchSeq != None:
                    #check that the sequecens lengths are same
                    if not (len(qSeq) == len(sbjSeq) == len(matchSeq)):
                        print('QUERY SEQ LENGTH:\t%s'%len(qSeq))
                        print('MATCH SEQ LENGTH:\t%s'%len(matchSeq))
                        print('SBJ SEQ LENGTH:\t%s'%len(sbjSeq))
                        sys.exit('ERROR: the length of all the  extracted sequences must be the same')
                    ##structStr = '%ds %ds I I I I I I I f I f %ds %ds %ds'%(len(qId), len(hId), len(qSeq), len(matchSeq), len(sbjSeq))
                    structStr = '%ds %ds I I I I I I I %ds I f %ds %ds %ds'%(len(qId), len(hId), len(evalue), len(qSeq), len(matchSeq), len(sbjSeq))
                    recordStruct = struct.Struct(structStr)
                    qCovHsp = round((float(alignLen)/float(qLen))*100, 2)
                    if debug:
                        print('QUERY LENGTH:\t%s'%qLen)
                        print('ALIGN LENGTH:\t%s'%alignLen)
                        print('QCOVHSP:\t%s'%qCovHsp)
                    ##packedRec = recordStruct.pack(str(qId), str(hId), alignLen, identities, gaps, qStart, qEnd, sbjStart, sbjEnd, evalue, bScore, qCovHsp, str(qSeq), str(matchSeq), str(sbjSeq))
                    packedRec = recordStruct.pack(str(qId), str(hId), alignLen, identities, gaps, qStart, qEnd, sbjStart, sbjEnd, evalue, bScore, qCovHsp, str(qSeq), str(matchSeq), str(sbjSeq))
                    #ADD THE RECORD TO THE DATABASE
                    recStr = '%s %s %s %s %s %s %s %s %s %s %s %s'%(qId, hId, str(alignLen), str(identities), str(gaps), qStart, qEnd, sbjStart, sbjEnd, str(evalue), str(bScore), qCovHsp)
                    if debug:
                        print('\nORIGINAL:\n%s'%recStr)
                        print('\nPACKED:\n%s'%str(packedRec))
                    #if debug:
                        #unpackedRec = recordStruct.unpack(packedRec)
                        #print '\nUNPACKED:\n%s'%str(unpackedRec)
                    dbRecId = '%s_%s_%s_%s_%s_%s_%s_%s'%(str(evalue), str(bScore), str(qCovHsp), hspCnt + 1, dbRecCnt, len(qId), len(hId), len(qSeq))
                    if debug:
                        print('DB REC ID:\t%s'%dbRecId)
                    if dbRecId in ctrlDict:
                        del mydb
                        sys.exit('id %s has been repeated!'%dbRecId)
                    else: ctrlDict[dbRecId] = None
                    #decide if you should write using batch mode or not
                    if batchMode:
                        #create the batch object if it does not exist yet
                        if batch == None:
                            #print('Creating batch object')
                            batch = leveldb.WriteBatch()
                        #incement the control flag to write the batch of instructions
                        bCnt +=1
                        batch.Put(dbRecId , packedRec)
                        if(bCnt == chunkSize): #write every chunkSize records are ready
                            #print('Writing %s...'%bCnt)
                            mydb.Write(batch, syncMode)
                            batch = None #reset the batch
                            bCnt = 0
                    else:
                        mydb.Put(dbRecId, packedRec, syncMode)
                    dbRecCnt +=1
                    matchSeq = None #reset the variable
                    dbRecId = None
                    structStr = recordStruct = packedRec = recStr = unpackedRec = dbRecId = None
                    ##################################
                readingHsp = qHitOpen = False
                if debug:
                    print('%s GOOD HITS FOR QUERY %s'%(str(goodHspCnt), qId))
                    print('%s EXTRACTED HITS FOR QUERY %s\n'%(str(hspParseChk), qId))
                    print('%s TOTAL HSP FOR QUERY %s'%(str(hspCnt), qId))
                    print('TOTAL RECORDS INSERTED IN DB %s'%(str(dbRecCnt)))
            #check that all the hsps had been extracted
                if goodHspCnt != hspParseChk:
                    if debug:
                        print('GOOD HITS %s'%(str(goodHspCnt)))
                        print('EXTRACTED HITS %s'%(str(hspParseChk)))
                        print('TOTAL HSP %s\n'%(str(hspCnt)))
                    sys.exit('Some of the hsp had not been extracted...\nhsp found: %d\thsp extrated: %d'%(goodHspCnt, hspParseChk))
                else:
                    if debug:
                        print('\nOK!\n')
            hspParseChk = goodHspCnt = hspCnt = 0
            qId = ln
        elif (ln[5:7] == 'h=') and (not readingHitDef):
            qLen = int(ln[7:-1])
            ##print 'QUERY LENGTH %s'%(qLen)
        elif ln[0] == '*': #NO HITS FOR THE QUERY
            HspCnt = 0
            qHitOpen = False
            readingHitDef = False
            ##print ln
        elif ln[:2] == 'Se':
            if debug:
                print(qId)
            qHitOpen = True
            continue
        #USE THE FLAGS TO EXTRACT INFO
        if qHitOpen:
            if (ln[:2] == '  ') and (hspParseChk == 0):
                if debug:
                    print(ln.strip())
                goodHspCnt += 1
            elif ln[0] == '>': #then we found an hsp (hit)
                if debug:
                    print('\n\n##### HIT FOUND ######')
                hId = ln[2:-1]
                hspParseChk += 1
                readingHitDef = True
                readingHsp = False
                continue
            #let's now keep reading the hit name
            if readingHitDef:
                if ln[5:7] == 'h=':
                    hitLen = int(ln[7:-1])
                    readingHitDef = False
                else: #let's add other parts of the hit definition
                    hId = hId + ln[0:-1]
            else: #let's extract the alignments and other info
                if ln[:3] == ' Sc': #then we found the line with Score, bitscore and evalue
                    #PACK THE RECORD AND STORE IT IN DATABASE
                    #NORMAL PACK
                    structStr = recordStruct = packedRec = recStr = unpackedRec = dbRecId = None
                    if matchSeq != None:
                        #check that the sequecens lengths are same
                        if not (len(qSeq) == len(sbjSeq) == len(matchSeq)):
                            print('QUERY SEQ LENGTH:\t%s'%len(qSeq))
                            print('MATCH SEQ LENGTH:\t%s'%len(matchSeq))
                            print('SBJ SEQ LENGTH:\t%s'%len(sbjSeq))
                            sys.exit('ERROR: the length of all the  extracted sequences must be the same')
                        ##structStr = '%ds %ds I I I I I I I f I f %ds %ds %ds'%(len(qId), len(hId), len(qSeq), len(matchSeq), len(sbjSeq))
                        structStr = '%ds %ds I I I I I I I %ds I f %ds %ds %ds'%(len(qId), len(hId), len(evalue), len(qSeq), len(matchSeq), len(sbjSeq))
                        recordStruct = struct.Struct(structStr)
                        qCovHsp = round((float(alignLen)/float(qLen))*100, 2)
                        if debug:
                            print('QUERY LENGTH:\t%s'%qLen)
                            print('ALIGN LENGTH:\t%s'%alignLen)
                            print('QCOVHSP:\t%s'%qCovHsp)
                        ##packedRec = recordStruct.pack(str(qId), str(hId), alignLen, identities, gaps, qStart, qEnd, sbjStart, sbjEnd, evalue, bScore, qCovHsp, str(qSeq), str(matchSeq), str(sbjSeq))
                        packedRec = recordStruct.pack(str(qId), str(hId), alignLen, identities, gaps, qStart, qEnd, sbjStart, sbjEnd, evalue, bScore, qCovHsp, str(qSeq), str(matchSeq), str(sbjSeq))
                        #ADD THE RECORD TO THE DATABASE
                        recStr = '%s %s %s %s %s %s %s %s %s %s %s %s'%(qId, hId, str(alignLen), str(identities), str(gaps), qStart, qEnd, sbjStart, sbjEnd, str(evalue), str(bScore), qCovHsp)
                        if debug:
                            print('\nORIGINAL:\n%s'%recStr)
                            print('\nPACKED:\n%s'%str(packedRec))
                        if debug:
                            unpackedRec = recordStruct.unpack(packedRec)
                            print('\nUNPACKED:\n%s'%str(unpackedRec))
                        dbRecId = '%s_%s_%s_%s_%s_%s_%s_%s'%(str(evalue), str(bScore), str(qCovHsp), hspCnt + 1, dbRecCnt, len(qId), len(hId), len(qSeq))
                        if debug:
                            print('DB REC ID:\t%s'%dbRecId)
                        if dbRecId in ctrlDict:
                            del mydb
                            sys.exit('id %s has been repeated!'%dbRecId)
                        else:
                            ctrlDict[dbRecId] = None
                        #decide if you should write using batch mode or not
                        if batchMode:
                            #create the batch object if it does not exist yet
                            if batch == None:
                                #print('Creating batch object')
                                batch = leveldb.WriteBatch()
                            #incement the control flag to write the batch of instructions
                            bCnt += 1
                            batch.Put(dbRecId , packedRec)
                            if bCnt == chunkSize: #write every chunkSize records are ready
                                #print('Writing %s...'%bCnt)
                                mydb.Write(batch, syncMode)
                                batch = None #reset the batch
                                bCnt = 0
                        else:
                            mydb.Put(dbRecId, packedRec, syncMode)
                        dbRecCnt +=1
                        structStr = recordStruct = packedRec = recStr = unpackedRec = dbRecId = None
                    if debug:
                        print('\n##### HSP FOUND ######')
                    #reset main variables
                    refSeqLen = qSeq = sbjSeq = matchSeq = qStart = qEnd = sbjStart = sbjEnd = qDir = sbjDir = identities = idPerc = gaps = alignLen = bScore = evalue = None
                    readingHsp = True #then we are reading the hsp and sequences
                    hspCnt += 1
                    flds = ln.split(' = ')
                    #extract the bitscore
                    #bScore = flds[1]
                    tmpFlds = flds[1].split('(') #then we need to remove the dx part to get the bitscore
                    tmpFlds = tmpFlds[-1].split(')')
                    bScore = int(tmpFlds[0])
                    del tmpFlds
                    #now extract the evalue
                    #evalue = float(flds[-1].strip())
                    evalue = flds[-1].strip()
                    if debug:
                        print('B-SCORE\t%s'%str(bScore))
                        print('E-VALUE\t%s'%str(evalue))
                    continue
                if readingHsp:
                    if ln[:3] == ' Id': #then we found the line with Identities, Identity Perc and Gaps
                        ##print ln
                        flds = ln.split(' = ')
                        #extract the bitscore
                        tmpFlds = flds[1].split('(') #then we need to remove the dx part to get the bitscore
                        #identity percentage
                        idPerc = tmpFlds[-1]
                        tmpFlds2 = idPerc.split('%')
                        idPerc = tmpFlds2[0].strip()
                        del tmpFlds2
                        #identitites and alignLength
                        tmpFlds2 = tmpFlds[0].split('/')
                        identities = int(tmpFlds2[0].strip())
                        alignLen = int(tmpFlds2[-1].strip())
                        del tmpFlds2
                        #gaps
                        gaps = flds[-1].split('/')
                        gaps = int(gaps[0].strip())
                        del flds
                        #LET'S CHECK WHAT WE HAVE EXTRACTED
                        if debug:
                            print('IDENTITY (%%)\t%s'%str(idPerc))
                            print('IDENTITITES\t%s'%str(identities))
                            print('ALIGN LENGTH\t%s'%str(alignLen))
                            print('GAPS\t%s'%str(gaps))
                    elif ln[:3] == ' St': #then we found the line the strands
                        tmpLn = ln[8:-1]
                        flds = tmpLn.split('/')
                        qDir = sbjDir = ''
                        if flds[0][0] == 'P': #then it is plus
                            qDir = '+'
                        else: qDir = '-'
                        #now the direction for the hit
                        if flds[1][0] == 'P': #then it is plus
                            sbjDir = '+'
                        else: sbjDir = '-'
                        if debug:
                            print('STRAND DIRECTIONS: %s/%s'%(qDir, sbjDir))
                    #extract the sequences and matches
                    elif ln[:2] == 'Qu':
                        #remove the 'Query  ' part
                        if debug:
                            print(ln.strip())
                        ln = ln[7:-1]
                        flds = ln.split('  ')
                        if qStart == None:
                            qStart = int(flds[0].strip())
                        qEnd = int(flds[-1].strip())
                        refSeqLen = len(flds[-2].strip()) #this will be used as a reference for the length in order to extract the '|' describing the match
                        if qSeq != None:
                            qSeq = qSeq + flds[-2].strip()
                        else:
                            qSeq = flds[-2].strip()
                        if debug:
                            print('len=%s %s %s %s'%(len(qSeq), qStart, qSeq, qEnd))
                    #extract the matches and gaps
                    #elif (ln[:5] == '     ') or ('|' in ln):
                    elif  ('|' in ln) and ('>' not in ln):
                        if debug:
                            print(ln)
                            print('REFERENCE LENGTH: %s'%refSeqLen)
                        #remove the 'Query  ' part
                        if matchSeq != None:
                            matchSeq = matchSeq + ln[-refSeqLen-1:].rstrip('\n')
                        else:
                            matchSeq = ln[-refSeqLen-1:].rstrip('\n')
                        if debug:
                            print('len=%s %s'%(len(matchSeq), matchSeq))
                    #extract the hit sequence
                    elif ln[:2] == 'Sb':
                        if debug:
                            print(ln.strip())
                        #remove the 'Sbjct  ' part
                        ln = ln[7:-1]
                        flds = ln.split('  ')
                        #print flds
                        if sbjStart == None:
                            sbjStart = int(flds[0].strip())
                        sbjEnd = int(flds[-1].strip())
                        if sbjSeq != None:
                            sbjSeq = sbjSeq + flds[-2].strip()
                        else:
                            sbjSeq = flds[-2].strip()
                        if debug:
                            print('len=%s %s %s %s'%(len(sbjSeq), sbjStart, sbjSeq, sbjEnd))
        #if cnt == 100000:
            #break
    #write the last record...
    if batchMode:
        if debug:
            print('Writing last batch records chunk...')
        mydb.Write(batch, sync=True)
    if dbRecId != None:
        mydb.Put(dbRecId, packedRec, sync=True)
    #dbRecCnt +=1
    #check that the record is not a double entry
    '''
    if dbRecId != None:
        if dbRecId in ctrlDict:
            del mydb
            sys.exit('id %s has been repeated! (FINAL ENTRY!)'%dbRecId)
    print('%s records in dictionary...'%str(len(ctrlDict)) )
    '''
    if mydb != None:
        del mydb
    if debug:
        print('CREATED DB:\n%s'%str(outDbPath))
        print('REC INSERTED IN DB:\t%s'%str(dbRecCnt))
    return outDbPath



def xmlToDB(inXml, outDbPath=None, overwrite=False, debug=False):
    '''
    This function will parse a blast generated xml output file
    and the needed information in a leveldb database
    '''
    from Bio.Blast import NCBIXML
    import leveldb
    import struct #to create a binary buffer to be stored in the database
    #define the output file path
    #stdHdr = '#qId\tsbjId\t%identity\talignLen\tmismatches\tgaps\tqStart\tqEnd\tsbjStart\tsbjEnd\tevalue\tbscore\tqCovHsp'
    #set the db path
    if outDbPath == None:
        outDbPath = os.path.basename(inXml)
        flds = outDbPath.split('.')
        if len(flds) > 1:
            outDbPath = flds[0] + '_db'
    outDbPath = os.path.dirname(inXml) + '/' + outDbPath
    #CREATE THE DATABASE
    if os.path.isdir(outDbPath):
        print('The database %s already exists'%outDbPath)
        if overwrite: #then destroy the database and recreate it
            print('The existing database %s will be deleted '%outDbPath)
            leveldb.DestroyDB(outDbPath)
        else:
            sys.stderr.write('Please remove the existing databse or se the overwrite flag to True')
    mydb = leveldb.LevelDB(outDbPath)
    ##### PARSE XML RESULTS #######
    xmlFd = open(inXml)
    blast_records = NCBIXML.parse(xmlFd)
    #E_VALUE_THRESH = 0.04 could be used to filter by evalue
    alignCnt = 0
    for blastRec in blast_records:
        qId = str(blastRec.query)
        if len(blastRec.alignments) >= 1:
            pass
        else:
            continue
        if debug:
            print('query:\t%s'%(qId))
            print('query_letter:\t%s'%(str(blastRec.query_letters)))
            print('len(blastRec.alignments):\t%s'%(str(len(blastRec.alignments))))
        alignCnt += 1
        totHits = 0
        dbRecCnt = 0
        for i, alignment in enumerate(blastRec.alignments):
            mTitle = alignment.title #match title
            mId = alignment.accession
            hitId = alignment.hit_id
            hitDef = alignment.hit_def
            #dbRecId = 'r'
            if debug:
                print('\n****Alignment****%s'%str(i))
                print('alignment ACC:', mId)
                print('matching_seq:', mTitle)
                print('HIT ID:', hitId)
                print('HIT DEF:', hitDef)
                print('matching_seq_length:', alignment.length)
            hits = len(alignment.hsps)
            totHits = totHits + hits
            for hsp in alignment.hsps:
                alignLen = int(hsp.align_length)
                identities = hsp.identities
                ##alignPerc = round(float(identities)/float(alignLen)*100, 2)
                qLength = int(blastRec.query_letters)
                gaps = int(hsp.gaps)
                ##mismatches = alignLen-identities-gaps
                qDirection, sbjDirection = hsp.frame # directions needed to calculate end positions
                qStart = hsp.query_start
                qEnd = hsp.query_end
                qSeq = hsp.query
                sbjSeq = hsp.sbjct
                matchSeq = hsp.match
                sbjStart = hsp.sbjct_start
                sbjEnd = hsp.sbjct_end
                evalue = float(hsp.expect)
                bScore = int(hsp.bits)
                #qCovHsp = round((float(alignLen)/float(qLength))*100, 2)
                if debug:
                    print('\n**** HSP ****')
                    print('frame:', hsp.frame)
                    ##print('strands:', hsp.strand)
                    print('query start:\t%s'%(str(hsp.query_start)))
                    print('subject start:\t%s'%(str(hsp.sbjct_start)))
                    print('QEND:\t%s'%(qEnd))
                    print('SBJEND:\t%s'%(sbjEnd))
                    print('score:\t%s'%(str(hsp.score)))
                    print('bits:\t%s'%str(bScore))
                    print('e value:', evalue)
                    print('num_alignments:', hsp.num_alignments)
                    print('identities:', identities)
                    print('positives:', hsp.positives) #positives
                    ##print('mismatches:', mismatches)
                    print('gaps:', gaps)
                    ##print('Alignment Perc:\t%s'%(alignPerc))
                    ##print('HSP Query Coverage Perc (qcovhsp):\t%s'%(qCovHsp))
                #PACK THE RECORD AND STORE IT IN DATABASE
                #NORMAL PACK
                #object of type 'int' has no len()
                structStr = '%ds %ds I I I I I I I f I %ds %ds %ds'%(len(qId), len(hitDef), len(qSeq), len(matchSeq), len(sbjSeq))
                recordStruct = struct.Struct(structStr)
                packedRec = recordStruct.pack(str(qId), str(hitDef), int(alignLen), int(identities), int(gaps), qStart, qEnd, sbjStart, sbjEnd, evalue, bScore, str(qSeq), str(matchSeq), str(sbjSeq))
                dbRecCnt +=1
                #ADD THE RECORD TO THE DATABASE
                recStr = '%s %s %s %s %s %s %s %s %s %s %s'%(qId, hitDef, str(alignLen), str(identities), str(gaps), qStart, qEnd, sbjStart, sbjEnd, str(evalue), str(bScore))
                #print '\nORIGINAL:\n%s'%recStr
                #print '\nPACKED:\n%s'%str(packedRec)
                unpackedRec = recordStruct.unpack(packedRec)
                #print '\nUNPACKED:\n%s'%str(unpackedRec)
                dbRecId = '%s_%s_%s_%s_%s_%s_%s'%(str(evalue), str(bScore), alignCnt, dbRecCnt, len(qId), len(hitDef), len(qSeq))
                if debug:
                    print( 'DB REC ID:\t%s'%dbRecId)
                mydb.Put(dbRecId, packedRec, sync=True)
        if debug:
            print('total HITS for the query:\t%s\n'%str(totHits))
        #break
        ##fdOut.write('#HITS\t%s\n'%str(totHits))
    if debug: #print stats about the database
        print(mydb.GetStats())
        #print stats
    del mydb #close the database
    xmlFd.close()
    if debug:
        print('CREATED DB:\n%s'%str(outDbPath))
    return outDbPath



def unpackBlastHitFromLvDB(dbKey, dbObj=None, debug=False):
    '''
    Takes as input a db key and db object or path to a db
    returns a tuple with the hit corresponding to the id
    '''
    import leveldb
    import struct
    #open the database if needed
    tmpType = type(dbObj)
    if tmpType == 'leveldb.LevelDB':
        pass
    elif tmpType == 'str':
        if os.path.isdir(dbObj): #open the database
            if debug:
                print('A path to a database has been provided\n%s'%dbObj)
            dbObj = leveldb.LevelDB(dbObj)
    #get the object corresponding to the id
    binRec = dbObj.Get(dbKey)
    #prepare the struct to unpack the blast hit record
    #the struct must be of the following format
    #'%ds %ds I I I I I I I ds% I f %ds %ds %ds'%(query_id_len, sbj_id_len, evalue_len, seq_len, seq_len, seq_len)
    #create the struct object based on the id
    flds = dbKey.split('_')
    qIdLen = int(flds[-3])
    sbjIdLen = int(flds[-2])
    seqLen = int(flds[-1])
    evalLen = len(flds[0])
    '''
    if debug:
        print('qIdLen:\t%s'%qIdLen)
        print('sbjIdLen:\t%s'%sbjIdLen)
        print('seqLen:\t%s'%seqLen)
    '''
    ##structStr = '%ds %ds I I I I I I I f I f %ds %ds %ds'%(qIdLen, sbjIdLen, seqLen, seqLen, seqLen)
    structStr = '%ds %ds I I I I I I I %ds I f %ds %ds %ds'%(qIdLen, sbjIdLen, evalLen, seqLen, seqLen, seqLen)
    recStruct = struct.Struct(structStr)
    #now we can unpack the binary record
    unpackedRec = recStruct.unpack(binRec)
    if debug:
        print(unpackedRec)
    #qId, sbjId, alignLen, identities, gaps, qStart, qEnd, sbjStart, sbjEnd, evalue, bScore, qCovHsp, qSeq, matchSeq, sbjSeq = unpackedRec
    return unpackedRec



def test_blastn(debug=False):
    '''This function will test the run of blastn.'''
    blastDbsDict = getBlastDbs()
    blastDB = blastDbsDict['ncbi_viruses_ntv']
    querySeq = '/user/gen-info/salvocos/tmp/test_blast_tools/input/cw2144_virus.fa'
    outTestDir = '/user/gen-info/salvocos/tmp/test_blast_tools/blastn_execution/'
    #let's now run blastn
    tpl = blastn(querySeq, blastDB, outTestDir, None, 1e-6, '0', threads=64, debug=debug) #automatic name
    #TEST defined name output
    if debug:
        print(str(tpl))



def test_blastp(debug=False):
    '''This function will test the run of blastp.'''
    import pathogenFinder2.pf2 as pf2
    pf2BlastDbsDict = pf2.get_families_blast_dbs()
    blastDB = pf2BlastDbsDict['ecoli_blast']
    querySeq = '/user/gen-info/salvocos/tmp/test_blast_tools/input/cw2140_ecoli_prot.faa'
    #querySeq = '/user/gen-info/salvocos/tmp/test_blast_tools/input/cw2140_ecoli_prot_small.faa'
    outTestDir = '/user/gen-info/salvocos/tmp/test_blast_tools/blastp_execution/'
    #let's now run blastn
    tpl = blastp(querySeq, blastDB, outTestDir, None, 1e-20, '0', threads=64, debug=debug) #automatic name
    #TEST defined name output
    if debug:
        print(str(tpl))



def test_blastx(debug=False):
    '''This function will test the run of blastx.'''
    import pathogenFinder2.pf2 as pf2
    pf2BlastDbsDict = pf2.get_families_blast_dbs()
    blastDB = pf2BlastDbsDict['ecoli_blast']
    querySeq = '/user/gen-info/salvocos/tmp/test_blast_tools/input/cw2140_ecoli_genes.fna'
    outTestDir = '/user/gen-info/salvocos/tmp/test_blast_tools/blastx_execution/'
    #let's now run blastn
    tpl = blastx(querySeq, blastDB, outTestDir, None, 1e-20, '7', threads=64, debug=debug) #automatic name
    #TEST defined name output
    if debug:
        print(str(tpl))



def test_convertBlastOutput(debug=True):
    '''test blast output conversion.'''
    outDirTest = '/user/gen-info/salvocos/tmp/blast_test/'
    inAsn =  '/user/gen-info/salvocos/tmp/blast_test/cw2150_se_75_velvet_ecoli_vs_pf2_ecoli.asn'
    #outFmt = '6 qcovs qcovhsp'
    outFmt = '6'
    #convert the blast output
    convertBlastOutput(inAsn, outDir=outDirTest, outName=None, outFormat=outFmt, html=False, debug=debug)



def test_extractBlastnBestHits(debug=True):
    '''test the extraction of best hits from blastn generated output files.'''
    outTestDir = '/user/gen-info/salvocos/tmp/blast_test/'
    #inOutBlast = '/user/gen-info/salvocos/tmp/blast_test/ttss_am-19226_vs_GCA_000153785.out'
    inOutBlast = '/user/gen-info/salvocos/tmp/blast_test/ttss_am-19226_vs_GCA_000152465.out'
    extractBlastnBestHits(inOutBlast, outDir=outTestDir, outName=None, outLog=True, debug=debug)



def test_extractBlastnBestHitsKc(debug=True):
    '''test the extraction of best hits from blastn generated output files.'''
    outTestDir = '/user/gen-info/salvocos/tmp/test_blast_tools/blastn_extraction/'
    #inOutBlast = '%sinput/cw2139_clean_se_virus_vs_ntv_20150615.out'%outTestDir
    inOutBlast = '%sinput/cw2144_clean_pe_virus_vs_ntv_20150615.out'%outTestDir
    topHitPath, noHitPath, missingHitPath, summaryPath = extractBlastnBestHitsKc(inOutBlast, outDir=outTestDir, bestHitsLimit=1, outPrefix=None, outLog=debug, debug=debug)



def test_extractBlastnBestHitsKcVirus(debug=True):
    '''test the extraction of best hits from blastn generated output files.'''
    outTestDir = '/user/gen-info/salvocos/tmp/test_blast_tools/blastn_extraction/'
    #inOutBlast = '%sinput/cw2139_clean_se_virus_vs_ntv_20150615.out'%outTestDir
    inOutBlast = '%sinput/cw2144_clean_pe_virus_vs_ntv_20150615.out'%outTestDir
    topHitPath, noHitPath, missingHitPath, summaryPath = extractBlastnBestHitsKcVirus(inOutBlast, outDir=outTestDir, bestHitsLimit=1, outPrefix=None, outLog=debug, debug=debug)



def test_extractBlastnBestHitsVirusFromTable(debug=True):
    '''test the extraction of best hits from blastn generated output files.'''
    outTestDir = '/user/gen-info/salvocos/tmp/test_blast_tools/blastn_extraction/'
    #inOutBlast = '%sinput/cw2139_clean_se_virus_vs_ntv_20150615.out'%outTestDir
    inOutBlast = '%sinput/cw2144_clean_pe_virus_vs_ntv_20150615.out'%outTestDir
    topHitPath, noHitPath, missingHitPath, summaryPath = extractBlastnBestHitsVirusFromTable(inOutBlast, outDir=outTestDir, bestHitsLimit=1, outPrefix=None, outLog=True, debug=debug)



def test_getNcbiLineageKc(debug=True):
    '''test lineage extraction'''
    #gi = '336284682'
    #getNcbiLineageKc(gi, debug=debug)
    gi = '823963021'
    getNcbiLineageKc(gi, debug=debug)



def test_getNcbiLineageVirusFromTable(debug=True):
    '''test lineage extraction'''
    #gi = '336284682'
    #getNcbiLineageKcVirus(gi, debug=debug)
    #gi = '823963021'
    gi = '1304465'
    outDict, found = getNcbiLineageKcVirus(gi, debug=debug)
    outLn = '\t'.join(map(str, outDict.values()))
    print('TAX FOUND:\t%s'%found)
    print(outLn)



def test_getNcbiLineageKcVirus(debug=True):
    '''test lineage extraction'''
    #gi = '336284682'
    #getNcbiLineageKcVirus(gi, debug=debug)
    #gi = '823963021'
    gi = '1304465'
    getNcbiLineageVirusFromTable(gi, taxDict=None, debug=debug)
    outLn = '\t'.join(map(str, outDict.values()))
    print('TAX FOUND:\t%s'%found)
    print(outLn)



def test_makedb(debug=False):
    '''This function will test the blast db creation.'''
    #input
    inSeq = '/user/gen-info/salvocos/test_directory/palcalifaciens_monash_3/input/invc_protein.fasta'
    #inSeqNt = '/user/gen-info/salvocos/test_directory/palcalifaciens_monash_3/input/invc_dna.fasta'
    inSeqNt = '/user/gen-info/salvocos/test_directory/saruNinjaBacteria/CW3191_repeats/geneprediction/metagenemark/meta-velvetg.contigs_mgm.fna'
    outDir = '/user/gen-info/salvocos/test_directory/blast_test/db/'
    #let's create the database
    makedb(inSeq, outDir, None, 'prot', debug) #automatic name
    #TEST defined name output
    makedb(inSeqNt, outDir, None, 'nucl', debug) #automatic name
    #TEST defined wrong dbtype
    makedb(inSeq, outDir, None, 'pro', debug) #automatic name



def test_xmlToBlastFmt7(inXml, outDir=os.getcwd(), debug=False):
    #inXml, outDir=os.getcwd(), debug=False
    '''Test conversion from xml to tab separated format.'''
    blastFilePath = '/user/gen-info/salvocos/test_directory/blast_test/tnphoa_vs_imet_bacteria_fmt5.xml'
    blastFilePath = '/user/gen-info/salvocos/test_directory/saruNinjaBacteria/CW3191_repeats/blast/cw3191_rep_mvelvet_mgm_VS_imet_virus.xml'
    #convert the output
    xmlToBlastFmt7(blastFilePath, os.path.dirname(blastFilePath), debug)



def test_xmlToDB(debug=False):
    #xmlToDB(inXml, outDbPath=None, overwrite=False debug=False):
    '''Test conversion from xml to a leveldb database.'''
    #blastFilePath = '/user/gen-info/salvocos/test_directory/blast_test/tnphoa_vs_imet_bacteria_fmt5.xml'
    #blastFilePath = '/Users/salvocos/Desktop/tnphoa_vs_imet_bacteria_fmt5.xml'
    blastFilePath = '/user/gen-info/salvocos/test_directory/saruNinjaBacteria/CW3191_repeats/blast/cw3191_rep_mvelvet_mgm_VS_imet_virus.xml'
    #convert the output
    xmlToDB(blastFilePath, None, True, debug)



def test_paths_update(debug=False):
    #check the current path
    print(getRoot())
    print(getBinRoot())
    #print the tools paths
    for program in blastTools:
        print(program)
        print(blastTools[program])
    #set the new root path
    print('\nCHANGE PATHS:')
    newRoot = setRoot('/user/gen-info/salvocos/projects/metapatfinder')
    print(newRoot)
    print(getBinRoot())
    updateBlastToolsPaths(getBinRoot(), debug=debug)
    print(getBlastnDbsRoot())
    print(getBlastpDbsRoot())
    print(getResourceRoot())
    print('\nUPDATED BLAST TOOLS PATHS:')
    for program in blastTools:
        print(program)
        print(blastTools[program])
    #update the paths to the databases
    updateBlastDbPaths(getBlastnDbsRoot())
    print('\nUPDATED DATABASE PATHS:')
    for dbName in blastDbPaths:
        print(dbName)
        print(blastDbPaths[dbName])



def test_rawBlastOutToDB(debug=False):
    '''Test conversion extraction for raw blast output file and storing of information to a leveldb database.'''
    #blastFilePath = '/user/gen-info/salvocos/test_directory/saruNinjaBacteria/CW3191_repeats/blast/cw3191_rep_mvelvet_mgm_VS_imet_virus_fmt0.out'
    blastFilePath = '/user/gen-info/salvocos/test_directory/blast_test/cw3191_repeats_mgm_vs_imet_virus.out'
    #convert the output
    #rawBlastOutToDB(blastFilePath, 'minchia_out_blast_test_db', True, debug)
    rawBlastOutToDB(blastFilePath, None, True, 0, False, debug)
