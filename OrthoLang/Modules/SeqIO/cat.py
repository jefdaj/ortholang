#!/usr/bin/env python2

# Just a fancy cat with special handling for ortholang's <<emptywhatever>> files
# TODO disable fixEmptyFile for this one probably
# TODO and need to add fixList
# Usage: cat.py <outfile> <emptylink> <inlist>

from os.path import islink, realpath
from shutil  import copyfileobj
from sys     import argv

def cat(infile, wfd):
    # based on https://stackoverflow.com/a/27077437
    # note that infile is a path but wfd is an open file descriptor
    with open(infile, 'rb') as fd:
        #10MB per writing chunk to avoid reading big file into memory.
        copyfileobj(fd, wfd, 1024*1024*10)

# def is_empty(emptylink, filetotest):
#     # because ortholang deduplicates all the <<empty...>> files,
#     # theoretically we can just test if each thing to concat is the same
#     # as the example without opening it. any reason that would be unsafe?
#     return islink(filetotest) and realpath(filetotest) == realpath(emptylink)

def is_empty(filetotest):
    # this will be slower than the symlink-based approach above,
    # but that seems like it might not be working
    # TODO speed it up by only opening the file once and checking first line?
    with open(filetotest, 'r') as f:
        line = f.readline()
        return line.startswith('<<empty')

def main(outpath, inlist, emptylink):
    # print 'args:', outpath, emptylink, inlist
    all_empty = True
    with open(outpath, 'wb') as outfile:
        with open(inlist, 'r') as infiles:
            for infile in infiles:
                infile = infile.strip()
                # if not is_empty(emptylink, infile):
                if not is_empty(infile):
                    all_empty = False
                    cat(infile, outfile)
        if all_empty:
            cat(emptylink, outfile)

if __name__ == '__main__':
    main(*argv[1:])
