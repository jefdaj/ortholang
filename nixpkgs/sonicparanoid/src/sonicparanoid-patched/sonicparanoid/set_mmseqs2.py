# -*- coding: utf-8 -*-
"""Copies a the MMseqs2 binary inside the SonicParanoid root directory, and check the version compatibility."""
import os
import sys
import platform
from sonicparanoid import sys_tools as systools
from sonicparanoid.sonic_paranoid import check_gcc
from shutil import copy2
import fnmatch

########### FUNCTIONS ############
def get_params():
    """Parse and analyse command line parameters."""
    # create the possible values for sensitivity value
    # define the parameter list
    import argparse
    usage = '\nProvide the path to the directory in which MMseqs2 source code shipping with SonicParanoid will be stored.\n'
    parser = argparse.ArgumentParser(description='Copies the MMseqs2 binary file inside the SonicParanoid root directory, and checks the version compatibility',  usage='%(prog)s -o <MMSEQS_PATH>', prog='sonicparanoid-set-mmseqs2')
    #start adding the command line options
    parser.add_argument('-i', '--mmseqs-path', type=str, required=True, help='The path to the MMseqs2 binary file.', default=None)
    parser.add_argument('-d', '--debug', required=False, help='Output debug information.', default=False, action='store_true')
    args = parser.parse_args()
    return (args, parser)



def yes_or_no():
    """Prompt yes or no, to overwrite the binaries."""
    while "the answer is invalid":
        reply = str(input('MMseqs2 binaries are already installed in SonicParanoid\'s directory.\nDo you want to overwrite the installed version? (y/n):')).lower().strip()
        if reply[:1] == 'y':
            return True
        if reply[:1] == 'n':
            return False



########### MAIN ############

def main():

    # check that everything has been installed correctly
    root = os.path.dirname(os.path.abspath(__file__))
    root += '/'

    #Get the parameters
    args, parser = get_params()
    #set the required variables
    debug = args.debug
    # bin dir inside SonicParanoid's directory
    binDir = os.path.join(root, 'bin/')
    # path to the MMseqs2 binaries
    mmseqsPath = os.path.realpath(args.mmseqs_path)

    # Make some checks before extracting the package
    if not os.path.isfile(mmseqsPath):
        sys.stderr.write('\nERROR: the provided path was not found.')
        sys.stderr.write('\nPlease, provide a valid path to the compiled MMseqs2 binary file.\n')
        sys.exit(-5)

    overwrite = True
    # check if the binaries are already available
    internalPath = os.path.join(binDir, 'mmseqs')
    if os.path.isfile(internalPath):
        # ask the user if it ok to overwrite
        overwrite = yes_or_no()

    if overwrite:
        # remove the file if it exists
        if os.path.isfile(internalPath):
            os.remove(internalPath)
        copy2(mmseqsPath, internalPath)
        print('MMseqs2 binaries successfully copied from:\n{:s}\nto:\n{:s}\n'.format(mmseqsPath, binDir))
    else:
        print('The previously installed MMseqs2 binaries\n{:s}\nwill be used.'.format(internalPath))
        print('EXIT: nothing done.')
