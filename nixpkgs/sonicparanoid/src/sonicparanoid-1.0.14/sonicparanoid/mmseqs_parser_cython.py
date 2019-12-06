import os
import sys
from collections import OrderedDict
# import the cython module
import mmseqs_parser_c
from sh import sort


########### FUNCTIONS ############
def get_params():
    """Parse and analyse command line parameters."""
    import argparse
    parser_usage = '\nProvide an input file generated using mmseqs2 and converted into BLASTP compatible tab-separated format.\nOutputs a file of ortholog and paralogs that can be processed by InParanoid.\n'
    parser = argparse.ArgumentParser(description='mmseqs2 parser 0.1', usage=parser_usage)
    #start adding the command line options
    parser.add_argument('-i', '--input', type=str, required=False, help='Tab-separated alignment file generated using mmseqs2 and converted in BLASTP tab-separated format.\nNOTE: for details on the fields contained in the output refer to function inparanoid_like_parser', default=None)
    parser.add_argument('-q', '--query', type=str, required=False, help='FASTA file with the query proteins.', default=None)
    parser.add_argument('-t', '--db', type=str, required=False, help='FASTA file with the target proteins.', default=None)
    parser.add_argument('-o', '--output', type=str, required=True, help='File in which the output will be stored.', default=None)
    parser.add_argument('-c', '--cutoff', type=int, required=False, help='Cutoff score (sum of hsp bitscores for each hit) for alignments.', default=40)
    parser.add_argument('-d', '--debug', required=False, help='Output debug information.\nNOTE: this is not used when the input is STDIN.', default=False, action='store_true')
    args = parser.parse_args()
    return (args, parser)



def test_parser(debug=True):
    """Test a run of the parser."""
    rootDir = '/home/salvocos/tmp/test_ortholog_detection/test_run_multiproc/alignments/'
    mmseqBlastFile = os.path.join(rootDir, 'mmseqs2blast.homo_sapiens-homo_sapiens')
    inputRoot = '/home/salvocos/tmp/test_ortholog_detection/input_bigger/'
    faQueryFile = os.path.join(inputRoot, 'homo_sapiens')
    faTargetFile = os.path.join(inputRoot, 'homo_sapiens')
    outDir = rootDir
    outName = '{:s}_PARSER_TEST'.format(os.path.basename(mmseqBlastFile))
    scoreCoff = 40
    debug = debug

    if debug:
        print('test_parser :: START')
        print('MMseqs file:\t{:s}'.format(mmseqBlastFile))
        print('Query fasta:\t{:s}'.format(faQueryFile))
        print('Target fasta:\t{:s}'.format(faTargetFile))
        print('Output name:\t{:s}'.format(outName))

    mmseqs_parser_c.mmseqs_parser_f0_9flds(mmseqBlastFile, faQueryFile, faTargetFile, outDir=outDir, outName=outName, scoreCoff=scoreCoff, debug=debug)



########### MAIN ############
def main():
    #Get the parameters
    args = get_params()[0]
    debug = args.debug
    inputPath = '%s'%os.path.realpath(args.input)
    queryPath = targetPath = ''
    if args.query:
        queryPath = '%s'%os.path.realpath(args.query)
    if args.db:
        targetPath = '%s'%os.path.realpath(args.db)
    outPath = '%s'%os.path.realpath(args.output)
    cutoff = args.cutoff
    outName = os.path.basename(outPath)
    outDir = os.path.dirname(outPath)

    if debug:
        print('Alignment file:\t%s'%inputPath)
        print('Query file:\t%s'%queryPath)
        print('Target file:\t%s'%targetPath)
        print('Output file:\t%s'%outPath)
        print('Cutoff:\t%s'%str(cutoff))

    # Working version from MASTER
    qSeqLenPath = os.path.join(outDir, '{:s}.len'.format(os.path.basename(queryPath)))
    tSeqLenPath = os.path.join(outDir, '{:s}.len'.format(os.path.basename(targetPath)))

    ########## sort the BLAST output ###########
    # sort blast_output -k1,1 -k2,2 -k12nr > sorted_output
    bsName: str = os.path.basename(inputPath)
    sortPath: str = os.path.join(outDir, 'sorted_{:s}'.format(bsName))
    ofd = open(sortPath, 'w')
    sort(inputPath, '-k1,1', '-k2,2', '-k12nr', _out=ofd)
    ofd.close()
    # remove the unsorted output and rename
    os.remove(inputPath)
    os.rename(sortPath, inputPath)
    ############################################

    # Parse the MMseqs2 output
    mmseqs_parser_c.mmseqs_parser_f0_9flds(inputPath, qSeqLenPath, tSeqLenPath, outDir=outDir, outName=outName, scoreCoff=cutoff, debug=debug)


if __name__ == "__main__":
    main()
