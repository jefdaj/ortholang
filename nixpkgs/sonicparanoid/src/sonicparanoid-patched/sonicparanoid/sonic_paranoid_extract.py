# -*- coding: utf-8 -*-
'''Extract clusters from SonicParanoid's output.'''
import os
import sys
import platform
import subprocess
from typing import Dict, List, Callable, Any, cast
#### IMPORT TO GENERATE PyPi package
#'''
from sonicparanoid import process_output as po
#'''
####

#### IMPORTS TO RUN LOCALLY
'''
import process_output as po
#'''
####

import fnmatch
import numpy as np
import time

########### FUNCTIONS ############
def get_params() -> Any:
    """Parse and analyse command line parameters."""
    # define the parameter list
    import argparse
    sonicparanoid_usage: str = '\nProvide a table with ortholog groups generated using SonicParanoid.\n'
    parser = argparse.ArgumentParser(description='SonicParanoid-extract 0.1',  usage='%(prog)s -i <input-table> [options]', prog='sonicparanoid-extract')
    #start adding the command line options
    parser.add_argument('-i', '--input-table', type=str, required=True, help='Table with output groups generated using SonicParanoid. NOTE: the headers must not be modified or removed.', default=None)
    parser.add_argument('-o', '--output-directory', type=str, required=True, help='The directory in which the results will be stored.', default='')
    parser.add_argument('-fd', '--fasta-directory', type=str, required=False, help='Directory containing the proteome files in FASTA format.', default='')
    parser.add_argument('-minsp', '--min-sp', type=int, required=False, help='Exctract clusters with genes from at least --min-sp species. (Default=2)', default=0)
    parser.add_argument('-maxsp', '--max-sp', type=int, required=False, help='Exctract clusters with genes from at max --max-sp species. (Default=2)', default=0)
    parser.add_argument('-ids','--ids-list', type=str, required=False, help='Extract a list of clusters by their ids.\nNOTE: the ids should be separated by a comma (e.g., --ids-list 20,23,40,22); this parameter bypasses the --min-sp and max-sp.', default='')
    minConfDefault: float = 0.05
    parser.add_argument('-c', '--min-conf', type=float, required=False, help='Keep only orthologs with a confidence higher than --min-conf. (Default={:.2f})'.format(minConfDefault), default=minConfDefault)
    parser.add_argument('-f', '--fasta', required=False, help='Generate a FASTA for with the proteins in each cluster.', default=False, action='store_true')
    parser.add_argument('-mf', '--multiple-fasta', required=False, help='Generate a FASTA file for each species in each cluster. (implies --fasta)', default=False, action='store_true')
    parser.add_argument('-ac','--annot-cols', type=str, required=False, help='Column ids containing annotations for the new HDR.\nNOTE: the column positions should be separated by a comma (e.g., --annot-cols 1,2,4,5); this parameter bypasses.', default='')
    parser.add_argument('-af', '--annot-file', type=str, required=False, help='Table with tab-serataed fields with annotations.', default=None)
    parser.add_argument('-gci', '--gene-col-id', type=int, required=False, help='Column number of the annotation file containing the protein id.', default=-1)
    parser.add_argument('-d', '--debug', required=False, help='Output debug information.', default=False, action='store_true')
    # return list with params
    return parser.parse_args()



########### MAIN ############
def main() -> None:

    # check that everything has been installed correctly
    root: str = os.path.dirname(os.path.abspath(__file__))
    root += '/'

    #Get the parameters
    args = get_params()
    #start setting the needed variables
    debug: bool = args.debug
    inTbl: str = args.input_table
    inDir: str = ''
    outDir: str = ''
    if args.input_table is not None:
        inDir = '{:s}/'.format(os.path.realpath(args.input_table))
    if len(args.output_directory) > 0:
        outDir = '{:s}/'.format(os.path.realpath(args.output_directory))
    else:
        outDir = '{:s}/'.format(os.getcwd())

    minConf: float = args.min_conf

    # extract fasta
    getFasta: bool = args.fasta
    multiFasta: bool = args.multiple_fasta
    if multiFasta: # force getFasta to True
        getFasta = True
    # make sure the directory with proteomes exists
    fastaDir: str
    if getFasta:
        if len(args.fasta_directory) == 0:
            sys.stderr.write('\nERROR: you must specify the path to the directory containing the FASTA files for the analyzed species.\n')
            sys.exit(-5)
        else:
            fastaDir = '{:s}/'.format(os.path.realpath(args.fasta_directory))

    # set flag control the type of extraction
    byId: bool = True
    # extract min and max sp count
    minSp: int = args.min_sp
    maxSp: int = args.max_sp

    # extract the list of ids
    idsListRaw: str = args.ids_list
    idsList: List[str] = idsListRaw.strip(' ').split(',')

    # remove any empty entry
    idx: int = -1
    while True:
        try:
            idx = idsList.index('')
            del idsList[idx]
        except ValueError:
            idx = -1
            break

    if minSp + maxSp > 2: # at least one of the 2 values has been set
        if len(idsList) == 0:
            byId = False

    # now check the parameters regarding the annotation
    # flag that says if annotation should be performed
    annotate: bool = False

    # make sure the annotation file exists
    annotPath: str
    if args.annot_file:
        annotPath = os.path.realpath(args.annot_file)
        if not os.path.isfile(annotPath):
            sys.stderr.write('\nERROR: you must specify the path to the file containing the annotations.\n')
            sys.exit(-2)
        else:
            annotate = True

    # now check the annot-cols parameter
    # extract the list of ids
    colsListRaw: str = args.annot_cols
    colsList = colsListRaw.strip(' ').split(',')

    # remove any empty entry
    idx = -1
    while True:
        try:
            idx = colsList.index('')
            del colsList[idx]
        except ValueError:
            idx = -1
            break

    # convert column ids to integers
    tmpColsList = [int(x) for x in colsList]
    del colsList
    colsList: List[int] = tmpColsList
    del tmpColsList

    # check congruency of annot-cols other annotation related parameters
    if len(colsList) == 0:
            if annotate: # then no annotation column was provided
                sys.stderr.write('\nERROR: you must specify the position of at least one column in the annotation file\n{:s}\n'.format(annotPath))
                sys.exit(-5)
    else:
        if not annotate: # the annotation file was not specified
            sys.stderr.write('\nERROR: you must specify the path to the annotation file.\n')
            sys.exit(-5)

    # now check the gene-col-id parameter
    geneColId: int = args.gene_col_id
    if geneColId >= 0:
        if annotate:
            # make sure that the annotation column list does not contain only the gene-col-id
            if len(colsList) == 1:
                if colsList[0] == geneColId:
                    sys.stderr.write('\nERROR: the annotation column position must differ from the gene ID column position.\n')
                    sys.exit(-5)
    elif geneColId < 0: # This would not fail if genColId == -1
        if annotate:
            sys.stderr.write('\nERROR: you must specify an interger higher than 0 as the column position with the gene IDs in the annotation file.\n')
            sys.exit(-5)

    #if debug:
    print('\nSonicParanoid-extract will be executed with following the parameters:')
    print('Input ortholog groups table: {:s}'.format(inTbl))
    print('Output directory: {:s}'.format(outDir))
    print('Minimum ortholog confidence:\t{:.2f}'.format(minConf))
    if getFasta:
        print('Directory with the analyzed FASTA files: {:s}'.format(fastaDir))
    if byId:
        print('Clusters to be extracted:\t{:s}'.format(str(idsList)))

    # define stats output file
    statsPath: str = os.path.join(outDir, 'stats.tsv')
    # write stats about the groups table
    po.process_multisp_tbl(inTbl=inTbl, outPath=statsPath, debug=debug)

    # perform the extraction
    clstrDict: Dict[str, Dict[str, List[str]]] = {}
    if byId:
        clstrDict = po.extract_by_id(inTbl=inTbl, idList=idsList, minConf=minConf, debug=debug)
    else:
        clstrDict = po.extract_by_sp_cnt(inTbl=inTbl, min=minSp, max=maxSp, outDir=outDir, minConf=minConf, debug=debug)
    # extract the fasta sequence if required
    if getFasta:
        # check if the annotation should be included in the headers
        annotDict: Dict[str, List[List[str]]] = {}
        if annotate:
            annotDict = po.load_annotations(annotFile=annotPath,  geneIdCol=geneColId, annotCols=colsList, debug=debug)
            #sys.exit('DEBUG :: sonic_paranoid_extract')
        po.extract_fasta(clstrDict=clstrDict, fastaDir=fastaDir, outDir=outDir, multiFasta=multiFasta, annotationDict=annotDict, debug=debug)



if __name__ == "__main__":
    main()
