'''This module contains functions to output colored text in the terminal.'''
import sys
#from colorama import Fore, Back, Style


__module_name__ = 'Colored Output'
__source__ = 'colored_output.py'
__author__ = 'Salvatore Cosentino'
#__copyright__ = ''
__license__ = 'GPL'
__version__ = '0.1'
__maintainer__ = 'Cosentino Salvatore'
__email__ = 'salvo981@gmail.com'



def info():
    """This module contains functions to output colored text in the terminal."""
    print('MODULE NAME:\t{:s}'.format(__module_name__))
    print('SOURCE FILE NAME:\t{:s}'.format(__source__))
    print('MODULE VERSION:\t{:s}'.format(__version__))
    print('LICENSE:\t{:s}'.format(__license__))
    print('AUTHOR:\t{:s}'.format(__author__))
    print('EMAIL:\t{:s}'.format(__email__))



def colored_info(outLn: str, lnType: str='e', debug: bool=False) -> None:
    """Output a color line based on the type of information."""
    if debug:
        print('colored_info :: START')
        print('Input string:\t{:s}'.format(outLn))
        print('String type:\t{:s}'.format(lnType.strip().lower()[0]))
    # check that the type is valid
    lnType = lnType.strip().lower()[0] # type: str
    validTypes = ['s', 'e', 'w', 'i'] # type: List[str]
    if not lnType in validTypes:
        print('\x1b[5;30;41mError:\x1b[0m {:s} is not a valid type of output line.'.format(lnType))
        print('Please use one of the following:\n{:s}'.format(str(validTypes)))
    # let's not print the output line
    if lnType == 's':
        print('\x1b[5;30;42mSuccess:\x1b[0m {:s}'.format(outLn))
    elif lnType == 'e':
        print('\x1b[5;30;41mError:\x1b[0m {:s}'.format(outLn))
    elif lnType == 'w':
        print('\x1b[5;30;43mWarning:\x1b[0m {:s}'.format(outLn))
    elif lnType == 'i':
        print('\x1b[5;30;44mInfo:\x1b[0m {:s}'.format(outLn))


''' Use colorama
def colored_log(outLn: str, lnType: str='e', debug: bool=False) -> None:
    """Output a color line based on the type of information using colorama."""
    if debug:
        print('colored_log :: START')
        print('Input string:\t{:s}'.format(outLn))
        print('String type:\t{:s}'.format(lnType.strip().lower()[0]))
    # check that the type is valid
    lnType = lnType.strip().lower()[0] # type: str
    validTypes = ['s', 'e', 'w', 'i'] # type: List[str]
    if not lnType in validTypes:
        #print('\x1b[5;30;41mError:\x1b[0m {:s} is not a valid type of output line.'.format(lnType))
        #sys.stdout.write(Fore.RED + 'some red text')
        sys.stdout.write('{:s} {:s} is not a valid type of output line.'.format(Fore.RED, lnType))
        sys.stdout.write(Style.RESET_ALL)
        print('Please use one of the following:\n{:s}'.format(str(validTypes)))
    # let's not print the output line
    if lnType == 's':
        #print('\x1b[5;30;42mSuccess:\x1b[0m {:s}'.format(outLn))
        sys.stdout.write('\n{:s}{:s}SUCCESS: {:s}{:s}'.format(Back.GREEN, Fore.BLACK, Style.RESET_ALL, outLn))
    elif lnType == 'e':
        sys.stdout.write('\n{:s}{:s}ERROR: {:s}{:s}'.format(Back.RED, Fore.BLACK, Style.RESET_ALL, outLn))
    elif lnType == 'w':
        sys.stdout.write('\n{:s}{:s}WARNING: {:s}{:s}'.format(Back.YELLOW, Fore.BLACK, Style.RESET_ALL, outLn))
    elif lnType == 'i':
        sys.stdout.write('\n{:s}{:s}INFO: {:s}{:s}'.format(Back.CYAN, Fore.BLACK, Style.RESET_ALL, outLn))
'''


def light_on_dark(outLn: str, bgColor: str='e', debug: bool=False) -> None:
    """Print the input line with the select background color and black foreground."""
    if debug:
        print('colored_info :: START')
        print('Input string:\t{:s}'.format(outLn))
        print('Background color:\t{:s}'.format(bgColor.strip().lower()[0]))
    # check that the type is valid
    bgColor = bgColor.strip().lower() # type: str
    validColors = ['cyan', 'red', 'orange', 'green'] # type: List[str]
    if not bgColor in validColors:
        print('\x1b[5;30;41mError:\x1b[0m is not a valid type of output line.'.format(bgColor))
        print('Please use one of the following colors:\n{:s}'.format(str(validColors)))
    # let's not print the output line
    if bgColor == 'green':
        sys.stdout.write('\x1b[5;30;42m{:s}\x1b[0m'.format(outLn))
    elif bgColor == 'red':
        print('\x1b[5;30;41m{:s}\x1b[0m'.format(outLn))
    elif bgColor == 'orange':
        print('\x1b[5;30;43m{:s}\x1b[0m'.format(outLn))
    elif bgColor == 'cyan':
        print('\x1b[5;30;44m{:s}\x1b[0m'.format(outLn))
