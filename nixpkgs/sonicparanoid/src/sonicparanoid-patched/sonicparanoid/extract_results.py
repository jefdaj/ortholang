# -*- coding: utf-8 -*-
"""Process output clusters.
    E.g., extract specific clusters and related sequences.
    """





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
    parser = argparse.ArgumentParser(description='SonicParanoid 1.0',  usage='%(prog)s -i <INPUT_DIRECTORY> -o <OUTPUT_DIRECTORY>[options]', prog='sonicparanoid.py')
    #start adding the command line options
    parser.add_argument('-i', '--input-directory', type=str, required=True, help='Directory containing the proteomes (in FASTA format) of the species to be compared. NOTE: The file names MUST NOT contain the \'-\' nor \'.\' characters', default=None)
    parser.add_argument('-o', '--output-directory', type=str, required=True, help='The directory in which the results will be stored.', default=None)
    #parser.add_argument('-t', '--threads', type=int, required=False, help='Number of parallel 1-CPU jobs to be used. Default=4', default=4)
    parser.add_argument('-d', '--debug', required=False, help='Output debug information.', default=False, action='store_true')
    args = parser.parse_args()
    return (args, parser)



########### MAIN ############
def main():
    #Get the input parameters
    args, parser = get_params()
    #set the required variables
    debug = args.debug
    # output dir
    outDir = os.path.realpath(args.output_directory)




if __name__ == "__main__":
    main()
