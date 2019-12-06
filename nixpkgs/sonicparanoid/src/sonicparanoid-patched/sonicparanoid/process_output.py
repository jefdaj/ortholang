'''Functions to process the output from SonicParanoid.'''
import os
import sys
from typing import Dict, List, Deque
from collections import OrderedDict, deque
from Bio import SeqIO
#### IMPORT TO GENERATE PyPi package
from sonicparanoid import sys_tools as systools
####

#### IMPORTS TO RUN LOCAL   LY
# import sys_tools as systools
####

__module_name__ = 'Process output'
__source__ = 'process_outpuy.py'
__author__ = 'Salvatore Cosentino'
__license__ = 'GPL'
__version__ = '0.2'
__maintainer__ = 'Cosentino Salvatore'
__email__ = 'salvo981@gmail.com'



### FUNCTIONS ####
def info() -> None:
    """This module contains functions for the detection of orthologs."""
    print('MODULE NAME:\t%s'%__module_name__)
    print('SOURCE FILE NAME:\t%s'%__source__)
    print('MODULE VERSION:\t%s'%__version__)
    print('LICENSE:\t%s'%__license__)
    print('AUTHOR:\t%s'%__author__)
    print('EMAIL:\t%s'%__email__)



def extract_fasta(clstrDict: Dict[str, Dict[str, List[str]]], fastaDir: str, outDir: str, multiFasta: bool = False, annotationDict: Dict[str, List[List[str]]] = {}, debug: bool = False) -> None:
    """Extract FASTA sequences for echa cluster."""
    if debug:
        print('\nextract_fasta :: START')
        print('Cluster for which sequences will be extracted:\t{:d}'.format(len(clstrDict)))
        print('Directory with the species files: {:s}'.format(fastaDir))
        print('Output directory: {:s}'.format(outDir))
        print('Output multiple FASTA files: {:s}'.format(str(multiFasta)))
        print('Length of annotation dictionary: {:d}'.format(len(annotationDict)))

    annotate: bool = False
    if len(annotationDict) > 0:
        annotate = True

    # check the directory with the fasta files exist
    if not os.path.isdir(fastaDir):
        sys.stderr.write('\nERROR (file not found): you must provide a valid path to the directory containig the species files.\n')
        sys.exit(-2)
    else: # make sure it is not empty
        tmpList: List[str] = os.listdir(fastaDir)
        if len(tmpList) < 2:
            sys.stderr.write('\nERROR: the directory containig the species files must contain at least two FASTA files.\n')
            sys.exit(-5)

    # will contain the species names that are actually required
    requSpDict: Dict[str, str] = {}

    # create the list with required species files
    for clstr, sp2geneDict in clstrDict.items():
        for sp, orthoList in sp2geneDict.items():
            # only process if required
            if sp in requSpDict:
                continue
            else:
                # make sure there is at least one ortholog for the current species
                if len(orthoList) == 1: # it could be empty
                    if orthoList[0][0] == '*': # then it is empty
                        continue
            # add the specis to the dictionary
            tmpPath: str = os.path.join(fastaDir, sp)
            requSpDict[sp] = tmpPath
            if not os.path.isfile(tmpPath):
                sys.stderr.write('\nERROR (file not found): the species file for {:s} was not found at\n{:s}\nplease provide a valid path.\n'.format(sp, tmpPath))
                sys.exit(-2)

    # load all the sequences in a dictionary
    # example, tvaginalis -> geneXAB -> ATGTAGGTA
    seqsDict: Dict[str, Dict[str, str]] = {}
    for spFile, fastaPath in requSpDict.items():
        spName: str = os.path.basename(spFile)
        seqsDict[spName] = load_seqs_in_dict(fastaPath=fastaPath, debug=debug)

    # queue to contain the paths of each single
    tmpDq: Deque[str]
    # now generate the output files
    # separated in directories separated by cluster id
    # and each file named clustersId-species_name
    for clstr, sp2geneDict in clstrDict.items():
        # create the output directory
        tmpClstrDir: str = os.path.join(outDir, 'clstr{:s}/'.format(clstr))
        systools.makedir(tmpClstrDir)
        # now for each species extract the sequences
        if multiFasta: #write one fasta file for each species
            for sp, orthoList in sp2geneDict.items():
                # skip the creation of files if the cluster is empty
                if len(orthoList) == 1:
                    if orthoList[0][0] == '*':
                        continue
                tmpFastaName = 'clstr{:s}-{:s}.fasta'.format(clstr, sp)
                tmpOutPath = os.path.join(tmpClstrDir, tmpFastaName)
                ofd = open(tmpOutPath, 'w')
                # write the sequences
                for ortho in orthoList:
                    if annotate:
                        # create the header by merging the annotations
                        newHdr: str
                        if ortho in annotationDict: # sometimes no annotation is found!
                            annotLists = annotationDict[ortho]
                            newHdr = '|'.join([';'.join(l) for l in annotLists])
                            ofd.write('>{:s}\n'.format(newHdr))
                        else:
                            ofd.write('>{:s}\n'.format(ortho))
                    else:
                        ofd.write('>{:s}\n'.format(ortho))
                    # write the sequence
                    ofd.write('{:s}\n'.format(str(seqsDict[sp][ortho])))
                ofd.close()
        else: #write a single FASTA file
            tmpFastaName = 'clstr{:s}.fasta'.format(clstr)
            tmpOutPath = os.path.join(tmpClstrDir, tmpFastaName)
            ofd = open(tmpOutPath, 'w')
            for sp, orthoList in sp2geneDict.items():
                # skip the creation of files if the cluster is empty
                if len(orthoList) == 1:
                    if orthoList[0][0] == '*':
                        continue
                # write the sequences
                for ortho in orthoList:
                    if annotate:
                        # create the header by merging the annotations
                        newHdr: str
                        if ortho in annotationDict: # sometimes no annotation is found!
                            annotLists = annotationDict[ortho]
                            newHdr = '|'.join([';'.join(l) for l in annotLists])
                            ofd.write('>{:s}\n'.format(newHdr))
                        else:
                            ofd.write('>{:s}\n'.format(ortho))
                    else:
                        ofd.write('>{:s}\n'.format(ortho))
                    # write the sequence
                    ofd.write('{:s}\n'.format(str(seqsDict[sp][ortho])))
            ofd.close()



def extract_by_id(inTbl: str, idList: List[str] = [], outDir: str = os.getcwd(), minConf: float = 0.1, debug: bool = False) -> Dict[str, Dict[str, List[str]]]:
    """Extract clusters based on on the number of species of which they are composed."""
    if debug:
        print('\nextract_by_id :: START')
        print('Input groups table:\t{:s}'.format(inTbl))
        print('Number of clusters to be extracted:\t{:d}'.format(len(idList)))
        print('IDs to be extracted:\t{:s}'.format(str(idList)))
        print('Output directory: {:s}'.format(outDir))
        print('Minimum confidence for orthologs:\t{:.2f}'.format(minConf))

    #check that the input directory is valid
    if not os.path.isfile(inTbl):
        sys.stderr.write('\nERROR (file not found): you must provide a valid path to the text file containig the ortholog groups table generated using SonicParanoid.\n')
        sys.exit(-2)
    # Check that ar least one id is in the list
    if len(idList) == 0:
        sys.stderr.write('\nERROR: you must provide at one cluster ID to be extracted, while you have provided none.\n')
        sys.exit(-5)
    # check that there are no repeated IDs in the ID list
    tmpDict: Dict[str, None] = {}
    tmpList: List[str] = []
    for el in idList:
        if not el in tmpDict:
            tmpDict[el] = None
        else:
            tmpList.append(el)
    # remove the repeated IDs if required
    if len(tmpList) > 0:
        for el in tmpList:
            idList.remove(el)
        sys.stderr.write('\nWARNING: the following cluster IDs were repeated in the input ID list and were removed.')
        sys.stderr.write('\n{:s}'.format(str(tmpList)))
        sys.stderr.write('\nThe ID list now contains {:d} cluster IDs.\n\n'.format(len(idList)))
    # remove the tmp structure
    del tmpDict
    tmpList.clear()

    # start processing the ortholog groups
    fd = open(inTbl, 'r')
    # extract the header and check the validity of the input file
    hdr_columns: List[str] = fd.readline().rstrip('\n').split('\t')
    # check the hdr
    if not hdr_columns[0] == 'group_id':
        sys.stderr.write('\nERROR: {:s}\nis not a valid header.\n')
        sys.exit('Make sure that the ortholog groups file was generated using SonicParanoid.')
    # extract the species count
    spCntStr: str = str(len(hdr_columns[4:-1])/2)
    spCntStr = spCntStr.strip()
    # check that the number of species is valid, for example not column was removed from the file
    # in thise case the dictionary must give a float with ending with '.0'
    if not spCntStr.endswith('.0'):
        sys.stderr.write('\nERROR : there is a problem with the number of species found in the table.\nMake sure you did not manually remove any column from the original output.\n')
        sys.exit(-2)
    # convert the string to int
    spCnt: int = int(spCntStr.split('.', 1)[0])
    # extract the species list
    spList: List[str] = [] # will contain the species names
    for i, el in enumerate(hdr_columns[4:-1]):
        if i % 2 == 0:
            spList.append(el)

    # prepare the output file
    outPath: str = os.path.join(outDir, 'filtered_{:s}'.format(os.path.basename(inTbl)))
    # create the output directory if required
    systools.makedir(outDir)
    ofd = open(outPath, 'w')
    # write the header
    ofd.write('{:s}\n'.format('\t'.join(hdr_columns)))

    # output dictionary and other variables
    # example: clst105 -> tvaginalis -> [g1, g4, g5]
    outDict: Dict[str, Dict[str, List[str]]] = {}
    extractedClstrCnt: int = 0
    extractedGenesCnt: int = 0
    totCnt: int = 0
    tmpSp: str = ''

    # start looping through the clusters
    for clstr in fd:
        flds: List[str] = clstr.rstrip('\n').split('\t')
        totCnt += 1
        clstrId: str = flds[0]
        # extract the information from the cluster
        if clstrId in idList:
            # write the filtered output file
            ofd.write(clstr)
            # keep only the usable fields
            flds = flds[4:-1]
            # add the id to output dictionary
            outDict[clstrId] = {}
            for i, el in enumerate(flds):
                # extract the cluster
                if i % 2 == 0:
                    # example of cluster
                    # 2336_Q9X2I8,2336_Q9X172:0.159
                    # create the list for the species
                    tmpSp = spList[int(i/2)]
                    outDict[clstrId][tmpSp] = []
                    for ortho in el.split(','):
                        tmpFlds: List[str] = ortho.split(':')
                        tmpConf: float
                        # case in which multuple columns are the in the gen name
                        if len(tmpFlds) > 3:
                            if ortho[-1] == ':':
                                # for example, x1ab:
                                # it is an ortholog for sure
                                outDict[clstrId][tmpSp].append(ortho)
                                if outDict[clstrId][tmpSp][-1][0] != '*':
                                    extractedGenesCnt += 1
                                continue
                            else: # the final field is the confidence
                                if float(tmpFlds[-1]) >= minConf:
                                    # extract and append the gene name
                                    outDict[clstrId][tmpSp].append(':'.join(tmpFlds[:-1]))
                                    if outDict[clstrId][tmpSp][-1][0] != '*':
                                        extractedGenesCnt += 1
                                    continue
                        else: # simpler case
                            if len(tmpFlds) == 2:
                                if float(tmpFlds[-1]) >= minConf:
                                    outDict[clstrId][tmpSp].append(tmpFlds[0])
                                    extractedGenesCnt += 1
                            else: # then the confidence must be 1.0
                                outDict[clstrId][tmpSp].append(tmpFlds[0])
                                if tmpFlds[0][0] != '*':
                                    extractedGenesCnt += 1
            # remove the ID from the list
            idList.remove(clstrId)
            # increase the count of extracted clusters
            extractedClstrCnt += 1
    fd.close()
    # close output file
    ofd.close()

    # print some debug line
    if debug:
        print('Extracted clusters:\t{:d}'.format(len(outDict)))
        if len(idList) > 0:
            print('(WARNING) The following clusters were not found: {:s}'.format(str(idList)))
        print('Extracted genes:\t{:d}'.format(extractedGenesCnt))
        print('Percentage of extracted clusters:\t{:.2f}'.format(round(float(extractedClstrCnt/totCnt) * 100., 2)))
    # return the main dictionary
    return outDict



def extract_by_sp_cnt(inTbl: str, min: int = 2, max: int = 2, outDir: str = os.getcwd(), minConf: float = 0.1, debug: bool = False) -> Dict[str, Dict[str, List[str]]]:
    """Extract clusters based on on the number of species of which they are composed."""
    if debug:
        print('\nextract_by_sp_cnt :: START')
        print('Input groups table:\t{:s}'.format(inTbl))
        print('Minimum number of species in cluster:\t{:d}'.format(min))
        print('Maximum number of species in cluster:\t{:d}'.format(max))
        print('Output directory: {:s}'.format(outDir))
        print('Minimum confidence for orthologs:\t{:.2f}'.format(minConf))
    #check that the input directory is valid
    if not os.path.isfile(inTbl):
        sys.stderr.write('\nERROR (file not found): you must provide a valid path to the text file containig the ortholog groups table generated using SonicParanoid.\n')
        sys.exit(-2)

    # check the minimum confidence value
    if not (0.05 <= minConf <= 1.):
        sys.stderr.write('\nWARNING: the ortholog confidence threshold must be set to a value between 0.05 and 1.0.\n')
        sys.stderr.write('It will now be set to 0.1.\n')
        min = max
    # start processing the ortholog groups
    fd = open(inTbl, 'r')
    # extract the head and check rthe validity of the input file
    hdr_columns: List[str] = fd.readline().rstrip('\n').split('\t')
    # check the hdr
    if not hdr_columns[0] == 'group_id':
        sys.stderr.write('\nERROR: {:s}\nis not a valid header.\n')
        sys.exit('Make sure that the ortholog groups file was generated using SonicParanoid.')
    spCntStr: str = str(len(hdr_columns[4:-1])/2)
    spCntStr = spCntStr.strip()
    # check that the number of species is valid, for example not column was removed from the file
    # in thise case the diction must give a float with ending with '.0'
    if not spCntStr.endswith('.0'):
        sys.stderr.write('\nERROR : there is a problem with the number of species found in the table.\nMake sure you did not manually remove any column from the original output.\n')
        sys.exit(-2)
    # convert the string to int
    spCnt: int = int(spCntStr.split('.', 1)[0])
    # More species requested than those avaliable in the input clusters
    if min > spCnt:
        sys.stderr.write('\nWARNING: {:d} species were found in the input table header, hence clusters with {:d} species cannot exist!.\n'.format(spCnt, max))
        sys.stderr.write('Both minimum and maximum will be set to ({:d}).\n'.format(spCnt))
        min = spCnt
        max = spCnt
    # min should lower than max!
    if min > max:
        sys.stderr.write('\nWARNING: the minimum number of species ({:d}) is higher than the maximum number of species ({:d}).\n'.format(min, max))
        sys.stderr.write('Max will be set to the maximum number of species in the table ({:d}).\n'.format(spCnt))
        max = spCnt

    # extract the species list
    spList: List[str] = [] # will contain the species names
    for i, el in enumerate(hdr_columns[4:-1]):
        if i % 2 == 0:
            spList.append(el)

    # prepare the output file
    if outDir[0] != '/':
        outDir += '/'
    outPath: str = os.path.join(outDir, 'filtered_min{:d}_max{:d}_{:s}'.format(min, max, os.path.basename(inTbl)))
    # create the output directory if required
    systools.makedir(outDir)
    ofd = open(outPath, 'w')
    # write the header
    ofd.write('{:s}\n'.format('\t'.join(hdr_columns)))

    # output dictionary
    # example: clst105 -> tvaginalis -> [g1, g4, g5]
    outDict: Dict[str, Dict[str, List[str]]] = {}
    extractedClstrCnt: int = 0
    extractedGenesCnt: int = 0
    totCnt: int = 0
    tmpSp: str = ''

    # start looping through the clusters
    for clstr in fd:
        flds: List[str] = clstr.rstrip('\n').split('\t')
        totCnt += 1
        clstrId: str = flds[0]
        spSize: int = int(flds[2])
        # check if it contains all species
        if min <= spSize <= max:
            # write the filtered output file
            ofd.write(clstr)
            # keep only the usable fields
            flds = flds[4:-1]
            # add the id to output dictionary
            outDict[clstrId] = {}
            for i, el in enumerate(flds):
                # extract the cluster
                if i % 2 == 0:
                    # example of cluster
                    # 2336_Q9X2I8,2336_Q9X172:0.159
                    # create the list for the species
                    tmpSp = spList[int(i/2)]
                    outDict[clstrId][tmpSp] = []
                    for ortho in el.split(','):
                        tmpFlds: List[str] = ortho.split(':')
                        tmpConf: float
                        # case in which multuple columns are the in the gen name
                        if len(tmpFlds) > 3:
                            if ortho[-1] == ':':
                                # for example, x1ab:
                                # it is an ortholog for sure
                                outDict[clstrId][tmpSp].append(ortho)
                                extractedGenesCnt += 1
                                continue
                            else: # the final field is the confidence
                                if float(tmpFlds[-1]) >= minConf:
                                    # extract and append the gene name
                                    outDict[clstrId][tmpSp].append(':'.join(tmpFlds[:-1]))
                                    extractedGenesCnt += 1
                                    continue
                        else: # simpler case
                            if len(tmpFlds) == 2:
                                if float(tmpFlds[-1]) >= minConf:
                                    outDict[clstrId][tmpSp].append(tmpFlds[0])
                                    extractedGenesCnt += 1
                            else: # then the confidence must be 1.0
                                outDict[clstrId][tmpSp].append(tmpFlds[0])
                                if tmpFlds[0][0] != '*':
                                    extractedGenesCnt += 1
            # increase the count of extracted clusters
            extractedClstrCnt += 1
    fd.close()
    # close output file
    ofd.close()

    # print some debug line
    if debug:
        print('Extracted clusters:\t{:d}'.format(len(outDict)))
        print('Extracted genes:\t{:d}'.format(extractedGenesCnt))
        print('Percentage of extracted clusters:\t{:.2f}'.format(round(float(extractedClstrCnt/totCnt) * 100., 2)))
    # return the main dictionary
    return outDict



def load_annotations(annotFile: str, geneIdCol: int = -1, annotCols: List[int] = [], debug: bool = False) -> Dict[str, List[List[str]]]:
    """Load annotations from annotation file"""
    if debug:
        print('\nload_annotations :: START')
        print('Column with gene ids:\t{:d}'.format(geneIdCol))
        print('Columns with annotations for the new header:\t{:s}'.format(str(annotCols)))
    # check the gene id and annotation column positions have been set
    if geneIdCol < 0:
        sys.stderr.write('\nERROR: the column index must be a positive integer.\n')
        sys.exit(-5)
    if len(annotCols) == 0:
        sys.stderr.write('\nERROR: you must provide at least one positive integer as position of the column with the annotation.\n')
        sys.exit(-5)

    # output dictionary
    outDict: Dict[str, List[List[str]]] = {}
    for ln in open(annotFile, 'r'):
        flds: List[str] = ln.rstrip('\n').split('\t')
        geneId = flds[geneIdCol]
        # extract
        annotListTmp: List[str] = [flds[pos] for pos in annotCols]
        # add the annotations in the dictionary
        if not geneId in outDict:
            outDict[geneId] = []
            for annot in annotListTmp:
                outDict[geneId].append([annot])
        else: # the sequence onctains multiple domains
            for idx, annot in enumerate(annotListTmp):
                #print(outDict[geneId])
                outDict[geneId][idx].append(annot)
                #print(outDict[geneId])
    return outDict



def load_seqs_in_dict(fastaPath: str, debug: bool = False) -> Dict[str, str]:
    """Load sequences for in a dictionary."""
    if debug:
        print('\nload_seqs_in_dict :: START')
        print('Proteome/Genome:\t{:s}'.format(fastaPath))
    # variables
    seqCnt: int = 0
    # write a pkl file with the lengths
    seqsDict: Dict[str, str] = {}
    # open sequence file
    for seq_record in SeqIO.parse(open(fastaPath), 'fasta'):
        seqsDict[seq_record.id] = seq_record.seq
        seqCnt += 1
    if debug:
        print('Loaded sequences for {:s}:\t{:d}'.format(os.path.basename(fastaPath), seqCnt))
    # return sequences
    return seqsDict



def process_multisp_tbl(inTbl: str, outPath: str, debug: bool = False) -> None:
    """Check consistency of table with ortholog groups and extract main stats."""
    if debug:
        print('process_multisp_tbl :: START')
        print('Input ortholog groups table:\t{:s}'.format(inTbl))
        print('Output stats file:\t{:s}'.format(outPath))
    #check that the input directory is valid
    if not os.path.isfile(inTbl):
        sys.stderr.write('\nERROR (file not found): you must provide a valid path to the text file containig the ortholog groups table generated using SonicParanoid.\n')
        sys.exit(-2)

    # create the directory that will contain the output file if required
    systools.makedir(os.path.dirname(outPath))

    # start processing the ortholog groups
    fd = open(inTbl, 'r')
    # extract the head and check rthe validity of the input file
    hdr_columns: List[str] = fd.readline().rstrip('\n').split('\t')
    # check the hdr
    if not hdr_columns[0] == 'group_id':
        sys.stderr.write('\nERROR: the header is not a valid.\n')
        sys.exit('Make sure that the ortholog groups file was generated using SonicParanoid.')
    spCntStr: str = str(len(hdr_columns[4:-1])/2)
    spCntStr = spCntStr.strip()
    # check that the number of species is valid, for example not column was removed from the file
    # in thise case the diction must give a float with ending with '.0'
    if not spCntStr.endswith('.0'):
        sys.stderr.write('\nERROR : there is a problem with the number of species found in the table.\nMake sure you did not manually remove any column from the original output.\n')
        sys.exit(-2)
    # convert the string to int
    spCnt: int = int(spCntStr.split('.', 1)[0])
    # variables to store the counts
    totCnt: int = 0
    allSpCnt: int = 0
    twoSpCnt: int = 0
    mostSeedsId: str
    maxSeedsCnt: int = 0
    # start looping through the clusters
    for clstr in fd:
        flds: List[str] = clstr.rstrip('\n').split('\t')
        totCnt += 1
        clstrId: str = flds[0]
        # check if it contains all species
        if int(flds[2]) == spCnt:
            allSpCnt += 1
        elif int(flds[2]) == 2:
            twoSpCnt += 1
        # find the cluster with the high amount of orthologs with confidence 1.0
        seedsCnt = int(flds[3])
        if seedsCnt > maxSeedsCnt:
            maxSeedsCnt = seedsCnt
            mostSeedsId = clstrId
    fd.close()
    # variables with allSp pct
    allSpPct: float = round(float(allSpCnt/totCnt) * 100., 2)
    twoSpPct: float = round(float(twoSpCnt/totCnt) * 100., 2)

    # open the output file
    ofd = open(outPath, 'w')
    ofd.write('Stats for the ortholog groups file:\n{:s}\n'.format(inTbl))
    ofd.write('\nClusters:\t{:d}'.format(totCnt))
    ofd.write('\nSpecies:\t{:d}'.format(spCnt))
    ofd.write('\nClusters with all species:\t{:d}'.format(allSpCnt))
    ofd.write('\nPercentage of clusters with all species:\t{:10.2f}'.format(allSpPct))
    ofd.write('\nClusters with two species:\t{:d}'.format(twoSpCnt))
    ofd.write('\nPercentage of clusters with two species:\t{:10.2f}'.format(twoSpPct))
    ofd.write('\nCluster with highest number of main orthologs:\t{:s}'.format(mostSeedsId))
    ofd.close()



def test_extract_by_id(debug: bool=True) -> None:
    """Test cluster extraction by cluster IDs."""
    pySrcDir: str = os.path.dirname(os.path.abspath(__file__))
    testRoot: str = os.path.join(pySrcDir, 'test/')
    outRoot: str = os.path.join(testRoot, 'output/')
    #fastaDir: str = os.path.join(testRoot, 'input/proteomes_missing/')
    fastaDir: str = os.path.join(testRoot, 'input/proteomes/')
    inputTbl: str = os.path.join(outRoot, 'multispecies_clusters.tsv')
    getFasta: bool = False
    #idList: List[str] = ['26', '4', '37', '780', '37']
    idList: List[str] = ['412']
    #idList: List[str] = ['23', 'MINCHIA']
    confThr: float = 1.0
    # extract the Clusters
    outClstrDict = extract_by_id(inTbl=inputTbl, idList=idList, outDir=outRoot, minConf=confThr, debug=debug)
    if debug:
        for k, val in outClstrDict.items():
            print(k, val)
    # now extract the FASTA sequences and generate the output FASTA files
    extract_fasta(clstrDict=outClstrDict, fastaDir=fastaDir, outDir=outRoot, debug=debug)



def test_extract_by_sp_cnt(debug: bool=True) -> None:
    """Test cluster extraction by species in cluster."""
    pySrcDir: str = os.path.dirname(os.path.abspath(__file__))
    testRoot: str = os.path.join(pySrcDir, 'test/')
    outRoot: str = os.path.join(testRoot, 'output/')
    #fastaDir: str = os.path.join(testRoot, 'input/proteomes_missing/')
    fastaDir: str = os.path.join(testRoot, 'input/proteomes/')
    inputTbl: str = os.path.join(outRoot, 'multispecies_clusters.tsv')
    minSp: int = 4
    maxSp: int = 4
    confThr: float = 0.8
    # extract the Clusters
    outClstrDict = extract_by_sp_cnt(inTbl=inputTbl, min=minSp, max=maxSp, outDir=outRoot, minConf=confThr, debug=debug)
    if debug:
        for k, val in outClstrDict.items():
            print(k, val)
    # now extract the FASTA sequences and generate the output FASTA files
    extract_fasta(clstrDict=outClstrDict, fastaDir=fastaDir, outDir=outRoot, debug=debug)



def test_extract_fasta(debug: bool=True) -> None:
    """Test FASTA sequence extraction for clusters."""
    pySrcDir: str = os.path.dirname(os.path.abspath(__file__))
    testRoot: str = os.path.join(pySrcDir, 'test/')
    outRoot: str = os.path.join(testRoot, 'output/')
    #fastaDir: str = os.path.join(testRoot, 'input/proteomes_missing/')
    fastaDir: str = os.path.join(testRoot, 'input/proteomes/')
    inputTbl: str = os.path.join(outRoot, 'multispecies_clusters.tsv')
    annotPath: str = os.path.join(outRoot, 'annotation_test_set.tsv')
    #idList: List[str] = ['26', '4', '37', '780', '37']
    idList: List[str] = ['23', '778']
    #idList: List[str] = ['778']
    confThr: float = 0.95
    # extract the Clusters
    outClstrDict = extract_by_id(inTbl=inputTbl, idList=idList, minConf=confThr, debug=debug)
    # now extract the FASTA sequences and generate the output FASTA files
    #extract_fasta(clstrDict = outClstrDict, fastaDir = fastaDir, outDir = outRoot, multiFasta=True, debug=debug)

    # generate an annotated single FASTA file
    #extract_fasta(clstrDict = outClstrDict, fastaDir = fastaDir, outDir = outRoot, multiFasta=False, debug=debug)

    #extract annotations
    annotDict = load_annotations(annotFile=annotPath,  geneIdCol=0, annotCols=[4, 5], debug=debug)
    extract_fasta(clstrDict = outClstrDict, fastaDir = fastaDir, outDir = outRoot, multiFasta=False, annotationDict=annotDict, debug=debug)



def test_load_seqs_in_dict(debug: bool=True) -> None:
    """Test the loading of sequence in a dictionary."""
    pySrcDir: str = os.path.dirname(os.path.abspath(__file__))
    testRoot: str = os.path.join(pySrcDir, 'test/')
    outRoot: str = os.path.join(testRoot, 'output/')
    fastaDir: str = os.path.join(testRoot, 'input/proteomes_missing/')
    #fastaDir: str = os.path.join(testRoot, 'input/proteomes/')
    inputFasta: str = os.path.join(fastaDir, 'gloeobacter_violaceus')
    # load sequences
    outSeqsDict = load_seqs_in_dict(fastaPath=inputFasta, debug=debug)
    # print some debug lines
    #for gene, seq in outSeqsDict.items():
        #print(gene, seq)
        #break



def test_load_annotations(debug: bool=True) -> None:
    """Test the loading the sequence annotations."""
    pySrcDir: str = os.path.dirname(os.path.abspath(__file__))
    testRoot: str = os.path.join(pySrcDir, 'test/')
    outRoot: str = os.path.join(testRoot, 'output/')
    annotPath: str = os.path.join(outRoot, 'annotation_test_set.tsv')

    # load sequences
    annotDict = load_annotations(annotFile=annotPath,  geneIdCol=0, annotCols=[4, 5], debug=debug)
    # print some debug lines
    lines: int = 20
    i: int = 0
    for gene, annotList in annotDict.items():
        print(gene, annotList)
        i += 1
        if lines == i:
            break



def test_process_multisp_tbl(debug: bool=True) -> None:
    """Test function to preprocess clusters."""
    pySrcDir: str = os.path.dirname(os.path.abspath(__file__))
    testRoot: str = os.path.join(pySrcDir, 'test/')
    outRoot: str = os.path.join(testRoot, 'output/')
    inputTbl: str = os.path.join(outRoot, 'multispecies_clusters.tsv')
    outPath: str = os.path.join(outRoot, 'newstats_{:s}'.format(os.path.basename(inputTbl)))
    # preprocess the Clusters
    process_multisp_tbl(inTbl=inputTbl, outPath=outPath, debug=debug)
