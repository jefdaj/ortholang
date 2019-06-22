# -*- coding: utf-8 -*-
"""Returns the MMseqs2 source code shipping with SonicParanoid."""
import os
import sys
import platform
from sonicparanoid import sys_tools as systools
from sonicparanoid.sonic_paranoid import check_gcc
import fnmatch

########### FUNCTIONS ############
def get_params():
    """Parse and analyse command line parameters."""
    # create the possible values for sensitivity value
    # define the parameter list
    import argparse
    usage = '\nProvide the path to the directory in which MMseqs2 source code shipping with SonicParanoid will be stored.\n'
    parser = argparse.ArgumentParser(description='Returns the source package of MMseqs2 supported by SonicParanoid',  usage='%(prog)s -o <OUTPUT_DIRECTORY>[options]', prog='sonicparanoid-get-mmseqs2')
    #start adding the command line options
    parser.add_argument('-o', '--output-directory', type=str, required=True, help='The directory in which the source files of MMseqs2 will be stored.', default=None)
    parser.add_argument('-d', '--debug', required=False, help='Output debug information.', default=False, action='store_true')
    args = parser.parse_args()
    return (args, parser)



def check_cmake():
    """Check that cmake is installed."""
    from sh import which
    shOut = which('cmake')
    if not shOut is None:
        from sh import cmake
        version = str(cmake('--version'))
        return (version, True)
    else:
        return (None, False)



def check_cmake3():
    """Check that cmake3 is installed."""
    from sh import which
    shOut = which('cmake3')
    if not shOut is None:
        from sh import cmake3
        version = str(cmake3('--version'))
        return (version, True)
    else:
        return (None, False)



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
    outDir = '{:s}/mmseqs2_src/'.format(os.path.realpath(args.output_directory))

    # Make some checks before extractic the package
    # check that GCC is installed
    gccVer, gccOk = check_gcc()
    if not gccOk:
        sys.stderr.write('\nERROR: no GCC compiler was found in your system.\n')
        sys.stderr.write('Please, go to https://gcc.gnu.org\n')
        sys.stderr.write('And follow the instructions to install the latest version of GCC in your system.\n')
        sys.exit(-5)
    # check that cmake is installed
    cmake3Ver, cmake3Ok = check_cmake3()
    cmakeVer, cmakeOk = check_cmake()

    # if None of the versions is installed then exit with an error
    if not (cmake3Ok or cmakeOk):
        print('ERROR: you must install cmake version 3.10 or above before continuing')
        sys.stderr.write('Please, go to https://cmake.org\n')
        sys.stderr.write('And follow the instructions on how to install the latest version of cmake in your system.\n')
        sys.exit(-5)

    # path to the source package
    tarPath = os.path.join(root, 'mmseqs2_src/mmseqs.tar.gz')

    # skip the extration if the directory already exists
    if os.path.isdir(outDir):
        print('WARNING: the directory\n{:s}'.format(outDir))
        print('already exists, if you want to extract the package,')
        print('please remove the above-mentioned directory.')
        print('\nEXIT: no file was extracted.')
        sys.exit(-2)

    # create the directory if it does not exist
    systools.makedir(outDir)

    #if debug:
    #    print('The source for MMseqs2 will be stored in\n{:s}'.format(outDir))
    systools.untar(tarPath, outDir=outDir, debug=debug)
    # create the build Directory
    systools.makedir(os.path.join(outDir, 'build/'))
