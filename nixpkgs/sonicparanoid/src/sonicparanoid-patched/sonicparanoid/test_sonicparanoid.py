'''This script expects that mmseqs and all the required modeles are properly instlled'''
import os
import sys
import subprocess

# import other modules
import ortholog_detection as orthodetect
import sys_tools as systools

root = '%s/'%os.path.dirname(orthodetect.__file__)
#print(root)

binDir = '%sbin/'%root
#if os.path.isdir(binDir):
#    print(binDir)

pckgDir = '%spackages/'%root
#if os.path.isdir(pckgDir):
#    print(pckgDir)

#sys.exit('DEBUG')




def check_mmseqs_installation():
    '''Check mmseqs installation'''
    print('Checking git installation...')
    shOut = which('git')
    print(shOut)
    if not shOut is None:
        from sh import git
        print(git('--version'))


def get_bin_dir():
    '''Return directory with binary files.'''
    return binDir



def get_pckg_dir():
    """Return the path to the directory with the third-party used software tools."""
    return pckgDir



def install_module(package):
    import importlib
    try:
        importlib.import_module(package)
    except ImportError as err:
        print(err)
        import pip
        pip.main(['install', package])



def check_module_installation(package):
    import importlib
    try:
        importlib.import_module(package)
    except ImportError as err:
        print(err)
        sys.exit('Please run setup_sonicparanoid.py and make sure all the required python3 modules and mmseqs2 are installed.')
    else:
        print('Module %s succesfully loaded!'%package)


########### MAIN ############

def main():
    import importlib
    import platform
    # check python version
    pver = sys.version_info

    # check python version
    print('You are using the following python version')
    print(sys.version)
    major = sys.version_info.major
    if major < 3:
        print('ERROR: You must use Python3 or above')
        print('Install Python3 then execute,')
        print('python3 setup_sonicparanoid')
        sys.exit(-5)
    print()

    # check operative system
    OS = platform.system()
    # retrieve the GCC compiler that will be used by python3
    pyCompiler = platform.python_compiler()
    osDist = None
    if OS == 'Linux':
        print('\nYou are using a %s system.'%OS)
        osDist = platform.linux_distribution()
        osDist = (osDist[0], osDist[1])
        print('\nDistribution:  %s %s'%(osDist[0], osDist[1]))
        pyCompiler = pyCompiler.split(' ')[1]
        #quickParanoidPckg = '%squickparanoid_redhat.tar.gz'%pckgDir
    elif OS == 'Darwin':
        #print('You are using a Apple MacOSX system')
        osDist = platform.mac_ver()
        osDist = (osDist[0])
        print('You are using a Apple MacOSX system, version %s'%osDist)
        pyCompiler = pyCompiler.split(' ')[1]
        #quickParanoidPckg = '%squickparanoid_osx.tar.gz'%pckgDir
    print('Your default GCC compiler has version\nGCC %s'%pyCompiler)


    # check if all modules were properly installed
    check_module_installation('sh')
    check_module_installation('numpy')
    check_module_installation('cython')
    check_module_installation('pandas')
    check_module_installation('Bio')
    check_module_installation('Bio')
    print('\nAll required python modules successfully loaded!\n')

    ### COMPILE CYTHON ###
    compile_cython()

    ### Check bundled mmseqs2 ###
    bundledOk, bundledPath = check_bundled_mmseqs(os=OS)

    # check if mmseqs2 is already installed
    from sh import which
    shOut = which('mmseqs')
    mmseqsInstallPath = None
    mmseqsInstalled = False
    if shOut is None:
        print('MMseqs2 not installed!')
    else:
        print('An installtion of MMseqs2 was found at')
        print(shOut)
        mmseqsInstalled = True
        mmseqsInstallPath = shOut.strip(' ')

    # if the bundled version is working suggest to copy to the working path
    if bundledOk:
        # move the binary file to the main bin directory
        print('The mmseqs binaries shipped shipping with SonicParanoid are properly working on your %s installation'%OS)
        print('We suggest you to include it to your binary path.\n')
        print('If you are using a bash terminal type (ON ANOTHER TERMINAl WINDOW):')
        if OS == 'Linux':
            if 'red hat' in osDist[0].lower():
                print('export PATH=%s:$PATH'%bundledPath)
            elif 'ubuntu' in osDist[0].lower():
                print('export PATH=%s:$PATH'%bundledPath)
        else: # OSX
            print('sudo cp -v %s /usr/local/bin/'%bundledPath)


    sys.exit('DEBUG')


    '''
    compileCmd = 'python3 compile_inpyranoid_c.py build_ext --inplace'
    process = subprocess.Popen(compileCmd, shell=True, stdout=subprocess.PIPE, stderr=subprocess.PIPE)
    stdout_val, stderr_val = process.communicate() #get stdout and stderr
    process.wait()
    print('\nCompile parser STDOUT:\n%s'%str(stdout_val.decode()))
    print('\nCompile parser STDERR:\n%s'%str(stderr_val.decode()))
    # compile mmseqs parser
    compileCmd = 'python3 compile_mmseqs_parser_c.py build_ext --inplace'
    process = subprocess.Popen(compileCmd, shell=True, stdout=subprocess.PIPE, stderr=subprocess.PIPE)
    stdout_val, stderr_val = process.communicate() #get stdout and stderr
    process.wait()
    print('\nCompile parser STDOUT:\n%s'%str(stdout_val.decode()))
    print('\nCompile parser STDERR:\n%s'%str(stderr_val.decode()))
    '''




if __name__ == "__main__":
    main()
