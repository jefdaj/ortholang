# -*- coding: utf-8 -*-
"""Returns the MMseqs2 source code shipping with SonicParanoid."""
import os
import sys
import platform
import shutil
from sonicparanoid import sys_tools as systools
import fnmatch

########### FUNCTIONS ############
def get_params():
    """Parse and analyse command line parameters."""
    # create the possible values for sensitivity value
    # define the parameter list
    import argparse
    usage = '\nProvide the path to the directory in which the test data for SonicParanoid will be stored.\n'
    parser = argparse.ArgumentParser(description='Created a test directory with test input proteomes for SonicParanoid',  usage='%(prog)s -o <OUTPUT_DIRECTORY>[options]', prog='sonicparanoid-get-test-data')
    #start adding the command line options
    parser.add_argument('-o', '--output-directory', type=str, required=True, help='The directory in which the test data will be stored.', default=None)
    parser.add_argument('-d', '--debug', required=False, help='Output debug information.', default=False, action='store_true')
    args = parser.parse_args()
    return (args, parser)



def copytree(src, dst, symlinks=False, ignore=None):
    for item in os.listdir(src):
        s = os.path.join(src, item)
        d = os.path.join(dst, item)
        if os.path.isdir(s):
            shutil.copytree(s, d, symlinks, ignore)
        else:
            shutil.copy2(s, d)



########### MAIN ############

def main():

    # check that everything has been installed correctly
    root = os.path.dirname(os.path.abspath(__file__))
    root += '/'

    #Get the parameters
    args, parser = get_params()
    #set the required variables
    debug = args.debug
    # output dir
    outDir = '{:s}/sonicparanoid_test/'.format(os.path.realpath(args.output_directory))

    # path to the source package
    testSrcDir = os.path.join(root, 'example/')

    # skip the extration if the directory already exists
    if os.path.isdir(outDir):
        print('WARNING: the directory\n{:s}'.format(outDir))
        print('already exists, if you want to extract the package,')
        print('please remove the above-mentioned directory.')
        print('\nEXIT: no file copied.')
        sys.exit(-2)
    # create the directory if it does not exist
    systools.makedir(outDir)

    # copy the test files
    print(outDir)
    copytree(testSrcDir, outDir, symlinks=False, ignore=None)
    #copytree(testSrcDir, outDir)
    if os.path.isdir(outDir):
        print('INFO: all test files were succesfully copied to\n{:s}\n'.format(outDir))
    # suggest the command to run
    print('Go inside the directory\n{:s}\nand type\n'.format(outDir))
    print('sonicparanoid -i ./test_input -o ./test_output -m fast -t 4')
