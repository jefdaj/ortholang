#!/usr/bin/env python

'''
Script to demo ShortCut, for example at a poster session.

Usage:
  shortcut-demo.py --help
  shortcut-demo.py -s <tmpsrc> -t <tmpdst> -c <script> [-b <binary>] [-w <wrapper>] [-d <workdir>]

Options:
  -h --help                Show this help text
  -s --tmpsrc <tmpsrc>     Source tmpdir with precomputed tmpfiles to speed things up.
  -t --tmpdst <tmpdst>     Destination tmpdir, where tmpdir files will be copied each run.
  -b --binary <binary>     Shortcut binary to run (defaults to system version)
  -c --script  <script>    Cut script to run, which should use the premade tmpfiles.
  -d --workdir <workdir>   Cut script to run, which should use the premade tmpfiles.
  -w --wrapper <wrapper>   Wrapper script to pass along for SLURM etc.
'''

from docopt import docopt
from subprocess import check_call
from time import sleep
from distutils.dir_util import mkpath
from os.path import exists, abspath, expanduser

def rsync(src, dst):
    src = abspath(expanduser(src)).rstrip('/') + '/*'
    dst = abspath(expanduser(dst)).rstrip('/')
    if not exists(dst):
        mkpath(dst)
    dst = dst + '/'
    cmd = ['rsync', '-aEvrz', '--delete', '--exclude=history.txt', src, dst]
    cmd = ' '.join(cmd)
    # print cmd
    check_call([cmd], shell=True)

def clear():
    'clears the terminal window'
    cmd = 'clear'
    check_call([cmd], shell=True)

def repl(args):
    cmd = [ args['--binary']
          , '--tmpdir', args['--tmpdst']
          , '--script', args['<script>']
          , '--interactive'
          ]
    if args['--wrapper']:
        cmd += ['--wrapper', args['--wrapper']]
    if args['--workdir']:
        cmd += ['--workdir', args['--workdir']]
    cmd = ' '.join(cmd)
    # print cmd
    # sleep(5)
    check_call([cmd], shell=True)

def demo(args):
    print args
    # sleep(5)
    while True:
        print 'resetting demo...'
        sleep(1)
        rsync(args['--tmpsrc'], args['--tmpdst'])
        try:
            clear()
            repl(args)
        except:
            sleep(1)

def main():
    args = docopt(__doc__, version='shortcut-demo 0.1')
    if not args['--binary']:
        args['--binary'] = 'shortcut'
    demo(args)

if __name__ == '__main__':
    main()
