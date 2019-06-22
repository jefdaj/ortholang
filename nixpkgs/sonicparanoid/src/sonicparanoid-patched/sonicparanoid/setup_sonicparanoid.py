import os
import sys
import subprocess
import json

# import other modules
#import ortholog_detection as orthodetect
import sys_tools as systools

root = '%s/'%os.path.dirname(systools.__file__)
#print(root)

binDir = '%sbin/'%root
#if os.path.isdir(binDir):
#    print(binDir)

quickMultiParaSrcDir = '%squick_multi_paranoid/'%root



def set_locale_to_utf8():
    '''Set locale to UTF-8 using export.'''
    #'export LC_ALL=LC_ALL.utf8'
    cmd = 'export LC_ALL=en_US.UTF-8'
    process = subprocess.Popen(cmd, shell=True, stdout=subprocess.PIPE, stderr=subprocess.PIPE)
    stdout_val, stderr_val = process.communicate() #get stdout and stderr
    process.wait()
    stdout_val = stdout_val.decode()
    stderr_val = stderr_val.decode()
    if len(stdout_val) > 0:
        print('\nset locale STDOUT:\n{:s}'.format(stdout_val))
    if len(stderr_val) > 0:
        print('\nset locale STDERR:\n{:s}'.format(stderr_val))



def check_mmseqs_installation(root):
    '''Check if mmseqs has been installed.'''
    #print('\n-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-')
    #print('Checking bundled mmseqs...')
    binDir = os.path.join(root, 'bin/')
    mmseqsPath = '{:s}mmseqs'.format(binDir)
    cmd = '%s'%mmseqsPath
    process = subprocess.Popen(cmd, shell=True, stdout=subprocess.PIPE, stderr=subprocess.PIPE)
    stdout_val, stderr_val = process.communicate() #get stdout and stderr
    process.wait()
    stdout_val = stdout_val.decode()
    stderr_val = stderr_val.decode()
    print('\nmmseqs2 STDOUT:\n{:s}'.format(stdout_val))
    #print(len(stdout_val))
    print('\nmmseqs2 STDERR:\n{:s}'.format(stderr_val))
    # if the is no error message
    if len(stderr_val) == 0:
        return (True, mmseqsPath)
    else:
        return (False, None)



#TODO this should use check for the brew gcc version instead of clang
def check_gcc(os='Linux'):
    '''Check that gcc is installed'''
    print('Check gcc (c++) compiler...')
    from sh import which
    shOut = which('gcc')
    print(shOut)
    if not shOut is None:
        from sh import gcc
        #print(gcc('--version'))
        return str(gcc('--version'))
    else:
        print('ERROR: you must install gcc version 5.1 or above before continuing')



def check_git(os='Linux'):
    '''Check git installation'''
    print('Checking git installation...')
    from sh import which
    shOut = which('git')
    print(shOut)
    if not shOut is None:
        from sh import git
        return str(git('--version'))



def compile_cython():
    '''Compile Cython modules using sh'''
    print('\n-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-')
    print('Compiling Cython code...')
    # compile inpyranoid
    from sh import python3
    shOut = python3('compile_inpyranoid_c.py', 'build_ext', '--inplace')
    print(shOut)
    # compile mmseqs_parser
    shOut = python3('compile_mmseqs_parser_c.py', 'build_ext', '--inplace')
    print(shOut)



def compile_cython_centos():
    '''Compile Cython modules (CentOS)'''
    print('Compiling Cython code...')
    # compile inpyranoid
    compileCmd = 'python3.6 compile_inpyranoid_c.py build_ext --inplace'
    process = subprocess.Popen(compileCmd, shell=True, stdout=subprocess.PIPE, stderr=subprocess.PIPE)
    stdout_val, stderr_val = process.communicate() #get stdout and stderr
    process.wait()
    print('\nCompile inpyranoid STDOUT:\n%s'%str(stdout_val.decode()))
    print('\nCompile inpyranoid STDERR:\n%s'%str(stderr_val.decode()))
    # compile mmseqs parser
    compileCmd = 'python3.6 compile_mmseqs_parser_c.py build_ext --inplace'
    process = subprocess.Popen(compileCmd, shell=True, stdout=subprocess.PIPE, stderr=subprocess.PIPE)
    stdout_val, stderr_val = process.communicate() #get stdout and stderr
    process.wait()
    print('\nCompile parser STDOUT:\n%s'%str(stdout_val.decode()))
    print('\nCompile parser STDERR:\n%s'%str(stderr_val.decode()))



def compile_quick_multiparanoid(srcDir):
    '''Compile the program for multi-species orthology'''
    print('\n-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-')
    print('Compiling quick multi-paranoid...')
    # compile inpyranoid
    from sh import make
    # move inot the src directory
    prevDir = os.getcwd()
    os.chdir(srcDir)
    # first remove any previously compiled file
    shOut = make('clean')
    print('Cleaning...')
    print(shOut)
    print('Make...')
    shOut = make('qa')
    print(shOut)
    # return to the pprevious directory
    os.chdir(prevDir)



def get_bin_dir():
    '''Return directory with binary files.'''
    return binDir



def install_and_import(package):
    import importlib
    try:
        importlib.import_module(package)
    except ImportError as err:
        print(err)
        import pip
        pip.main(['install', package])
    else:
        globals()[package] = importlib.import_module(package)



def install_module(package):
    import importlib
    try:
        importlib.import_module(package)
    except ImportError as err:
        print(err)
        import pip
        pip.main(['install', package])



def install_modules_fedora(debug=False):
    """Install the required modules on a Fedora distribution."""
    # install required modules
    cmd = 'sudo -H pip3 install -U pip setuptools sh cython numpy pandas biopython'
    process = subprocess.Popen(cmd, shell=True, stdout=subprocess.PIPE, stderr=subprocess.PIPE)
    stdout_val, stderr_val = process.communicate() #get stdout and stderr
    process.wait()
    print('\nFedora python3 modules install STDOUT:\n%s'%str(stdout_val.decode()))
    print('\nFedora python3 modules install STDERR:\n%s'%str(stderr_val.decode()))



def install_modules_centos(debug=False):
    """Install the required modules on a Fedora distribution."""
    # install required modules
    cmd = 'sudo -H pip3.6 install -U sh cython numpy pandas biopython'
    process = subprocess.Popen(cmd, shell=True, stdout=subprocess.PIPE, stderr=subprocess.PIPE)
    stdout_val, stderr_val = process.communicate() #get stdout and stderr
    process.wait()
    print('\nCentOS python3 modules install STDOUT:\n%s'%str(stdout_val.decode()))
    print('\nCentOS python3 modules install STDERR:\n%s'%str(stderr_val.decode()))



def install_modules_ubuntu(debug=False):
    """Install the required modules on a Ubuntu distribution."""
    # install required modules
    #cmd = 'sudo -H pip3 install -U sh pip setuptools cython'
    cmd = 'sudo -H pip3 install -U sh pip setuptools numpy pandas biopython'
    process = subprocess.Popen(cmd, shell=True, stdout=subprocess.PIPE, stderr=subprocess.PIPE)
    stdout_val, stderr_val = process.communicate() #get stdout and stderr
    process.wait()
    print('\nUbuntu python3 modules install STDOUT:\n%s'%str(stdout_val.decode()))
    print('\nUbuntu python3 modules install STDERR:\n%s'%str(stderr_val.decode()))



def install_modules_osx(debug=False):
    """Install the required modules on a OSX distribution."""
    # install required modules
    # Update/install pip
    modName = 'pip'
    cmd = 'pip3 install -U %s'%modName
    process = subprocess.Popen(cmd, shell=True, stdout=subprocess.PIPE, stderr=subprocess.PIPE)
    stdout_val, stderr_val = process.communicate() #get stdout and stderr
    process.wait()
    print('\n%s install STDOUT:\n%s'%(modName, str(stdout_val.decode())))
    print('\n%s install STDERR:\n%s'%(modName, str(stderr_val.decode())))

    # Update/install setuptools
    modName = 'setuptools'
    cmd = 'pip3 install -U %s'%modName
    process = subprocess.Popen(cmd, shell=True, stdout=subprocess.PIPE, stderr=subprocess.PIPE)
    stdout_val, stderr_val = process.communicate() #get stdout and stderr
    process.wait()
    print('\n%s install STDOUT:\n%s'%(modName, str(stdout_val.decode())))
    print('\n%s install STDERR:\n%s'%(modName, str(stderr_val.decode())))

    # Update/install Cython
    modName = 'cython'
    cmd = 'pip3 install -U %s'%modName
    process = subprocess.Popen(cmd, shell=True, stdout=subprocess.PIPE, stderr=subprocess.PIPE)
    stdout_val, stderr_val = process.communicate() #get stdout and stderr
    process.wait()
    print('\n%s install STDOUT:\n%s'%(modName, str(stdout_val.decode())))
    print('\n%s install STDERR:\n%s'%(modName, str(stderr_val.decode())))

    # Update/install numpy
    modName = 'numpy'
    cmd = 'pip3 install -U %s'%modName
    process = subprocess.Popen(cmd, shell=True, stdout=subprocess.PIPE, stderr=subprocess.PIPE)
    stdout_val, stderr_val = process.communicate() #get stdout and stderr
    process.wait()
    print('\n%s install STDOUT:\n%s'%(modName, str(stdout_val.decode())))
    print('\n%s install STDERR:\n%s'%(modName, str(stderr_val.decode())))

    # Update/install pandas
    modName = 'pandas'
    cmd = 'pip3 install -U %s'%modName
    process = subprocess.Popen(cmd, shell=True, stdout=subprocess.PIPE, stderr=subprocess.PIPE)
    stdout_val, stderr_val = process.communicate() #get stdout and stderr
    process.wait()
    print('\n%s install STDOUT:\n%s'%(modName, str(stdout_val.decode())))
    print('\n%s install STDERR:\n%s'%(modName, str(stderr_val.decode())))

    # Update/install biopython
    modName = 'biopython'
    cmd = 'pip3 install -U %s'%modName
    process = subprocess.Popen(cmd, shell=True, stdout=subprocess.PIPE, stderr=subprocess.PIPE)
    stdout_val, stderr_val = process.communicate() #get stdout and stderr
    process.wait()
    print('\n%s install STDOUT:\n%s'%(modName, str(stdout_val.decode())))
    print('\n%s install STDERR:\n%s'%(modName, str(stderr_val.decode())))

    # Update/install biopython
    modName = 'sh'
    cmd = 'pip3 install -U %s'%modName
    process = subprocess.Popen(cmd, shell=True, stdout=subprocess.PIPE, stderr=subprocess.PIPE)
    stdout_val, stderr_val = process.communicate() #get stdout and stderr
    process.wait()
    print('\n%s install STDOUT:\n%s'%(modName, str(stdout_val.decode())))
    print('\n%s install STDERR:\n%s'%(modName, str(stderr_val.decode())))



def check_module_installation(package):
    import importlib
    try:
        importlib.import_module(package)
    except ImportError as err:
        print(err)
        return False
    else:
        print('Module %s succesfully loaded!'%package)
        return True



########### MAIN ############

def main():
    import importlib
    import platform

    # set the locale to UTF-8
    # to avoid ascii enconding errors on some environments
    set_locale_to_utf8()

    # go to the directory in which this file is contained
    pySrcDir = os.path.dirname(os.path.abspath(__file__))
    pySrcDir += '/'
    # store current working directory
    prevDir = os.getcwd()
    # change to the directory with the source code
    os.chdir(pySrcDir)

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
        print(pyCompiler)
        pyCompiler = pyCompiler.split(' ')
        if len(pyCompiler) > 1:
            pyCompiler = pyCompiler[1]
        else:
            pyCompiler = pyCompiler[0]
    elif OS == 'Darwin':
        #print('You are using a Apple MacOSX system')
        osDist = platform.mac_ver()
        osDist = (osDist[0], None)
        print(osDist)
        print('You are using a Apple MacOSX system, version %s'%osDist[0])
        pyCompiler = pyCompiler.split(' ')[1]
    print('Your default GCC compiler has version\nGCC %s'%pyCompiler)

    # get the distribution
    distro = osDist[0]

    #Get the parameters
    print('Installing/updating Python3 modules required by SonicParanoid')

    # choose the installation depending on the distribution
    if OS == 'Linux':
        if 'opensuse' in distro.lower():
            install_modules_fedora(debug=True)
        elif 'centos' in distro.lower():
            install_modules_centos(debug=True)
        elif 'ubuntu' in distro.lower():
            install_modules_ubuntu(debug=True)
        else:
            install_modules_fedora(debug=True)
    elif OS == 'Darwin':
        install_modules_osx(debug=True)

    #print(distro.lower())
    #sys.exit('DEBUG')

    #''' INSTALL MODULES
    # check if all modules were properly installed
    modulesCheckList = {'numpy':False, 'cython':False, 'pandas':False, 'biopython':False, 'sh':False,}
    # Numpy
    modName = 'numpy'
    isOk = check_module_installation(modName)
    if not isOk:
        sys.stderr.write('\nERROR: %s Python3 module not properly installed.\nPlease install it on your system before proceeding.'%modName)
        sys.exit(-6)
    # Cython
    modName = 'cython'
    isOk = check_module_installation(modName)
    if not isOk:
        sys.stderr.write('\nERROR: %s module for Python3 not properly installed.\nPlease install it on your system before proceeding.'%modName)
        sys.exit(-6)
    # Pandas
    modName = 'pandas'
    isOk = check_module_installation(modName)
    if not isOk:
        sys.stderr.write('\nERROR: %s module for Python3 not properly installed.\nPlease install it on your system before proceeding.'%modName)
        sys.exit(-6)
    # Biopython
    modName = 'Bio'
    isOk = check_module_installation(modName)
    if not isOk:
        sys.stderr.write('\nERROR: Biopython module for python3 not properly installed.\nPlease install it on your system before proceeding.'%modName)
        sys.exit(-6)
    # SH
    modName = 'sh'
    isOk = check_module_installation(modName)
    if not isOk:
        sys.stderr.write('\nERROR: %s module for Python3 not properly installed.\nPlease install it on your system before proceeding.'%modName)
        sys.exit(-6)

    print('\nAll required python modules successfully loaded!\n')
    #'''

    ### COMPILE CYTHON ###
    compile_cython()

    ### COMPILE QUICK-MULTIPARANOID ###
    compile_quick_multiparanoid(quickMultiParaSrcDir)
    #sys.exit('DEBUG')

    ### Check bundled mmseqs2 ###
    bundledOk, bundledPath = check_bundled_mmseqs(os=OS)

    from sh import which
    mmseqsInstallPath = None
    mmseqsInstalled = False

    # if the bundled version is working suggest to copy to the working path
    if bundledOk:
        # move the binary file to the main bin directory
        print('The mmseqs binaries included with SonicParanoid are properly working on your %s computer'%OS)
        #print('We suggest you to include it to your binary path.\n')
        #print('If you are using a bash terminal type (ON ANOTHER TERMINAl WINDOW):')
        #'''
        if OS == 'Linux':
            if 'red hat' in osDist[0].lower():
                print('export PATH=%s:$PATH'%bundledPath)
            elif 'ubuntu' in osDist[0].lower():
                print('sudo cp -v %s /usr/local/bin/'%bundledPath)
                #print('export PATH=%s:$PATH'%bundledPath)
        else: # OSX
            print('sudo cp -v %s /usr/local/bin/'%bundledPath)
        #'''
        mmseqsInstallPath = bundledPath
    else:
        print('WARNING: The bundled MMseqs2 installation is not working.')
        print('Please provide the path to the MMseqs2 installation')


    # reset cwd
    os.chdir(prevDir)

    # set the values of the configuration
    print('\n-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-')
    print('Writing configuration in config.json ...')
    confDict = {}
    confDict['mmseqs'] = None
    if mmseqsInstallPath is None:
        confDict['mmseqs'] = ''
    else:
        confDict['mmseqs'] = mmseqsInstallPath
    # add other information
    # take the first 3 digits of the version number
    confDict['python'] = '.'.join(map(str, pver[0:3]))
    confDict['gcc'] = check_gcc(os=OS)
    confDict['git'] = check_git(os=OS)
    # write the JSON config file
    confFilePath = os.path.join(pySrcDir, 'config.json')
    with open(confFilePath, 'w') as outfile:
        json.dump(confDict, outfile)

    # check that the path to mmseqs is not empty
    if confDict['mmseqs'] == '':
        if mmseqsInstallPath is None:
            print('ERROR: the path to mmseqs was not set.')
            print('Please make sure that you built the mmseqs source (located in /src/mmseqs.tar.gz)\n')
            print('and that the binaries were copied in the /bin inside SonicParanoid\'s main directory.\n')
            print('Follow the instructions provided at http://iwasakilab.bs.s.u-tokyo.ac.jp/sonicparanoid')
            print('Once the mmseqs binaries are restored/copied in the /sonicparanoid/bin, please run setup_sonicparanoid.py again.\n')
            sys.exit('ERROR: MMseqs2 not properly installed/configured')
            '''
            if OS == 'Darwin':
                print('ERROR: the path to mmseqs was not set.')
                print('Please compile mmseqs for OSX as described in\n')
                print('http://iwasakilab.bs.s.u-tokyo.ac.jp/sonicparanoid')
                print('Once the mmseqs installation has been performed as described in the above web-page, do the following:\n')
                print('1) copy the mmseqs binaries under /sonicparanoid/bin/osx/ \n')
                print('2) Execute this script again (python3 setup_sonicparanoid.py) \n')
                sys.exit('MMseqs2 not properly installed/configured')
            else:
                print('ERROR: the path to mmseqs was not set.')
                print('Please make sure that you built the mmseqs source (located in /src/mmseqs.tar.gz)\n')
                print('and that the binaries were copied in the /bin inside SonicParanoid\'s main directory.\n')
                print('Follow the instructions provided at http://iwasakilab.bs.s.u-tokyo.ac.jp/sonicparanoid')
                print('Once the mmseqs binaries are restored/copied in the /sonicparanoid/bin, please run setup_sonicparanoid.py again.\n')
                sys.exit('ERROR: MMseqs2 not properly installed/configured')
            '''
    sys.exit('Installation complete!')




if __name__ == "__main__":
    main()
