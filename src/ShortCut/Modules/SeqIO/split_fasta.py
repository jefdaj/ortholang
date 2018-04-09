#!/usr/bin/env python2

# Split one multifasta file into multiple single-sequences ones.
# Names sequences sequentially to avoid any quoting issues.
# Usage: split_fasta.py <outdir> <infasta>

# TODO any issues with relative filepaths? should be OK with Cwd

from sys     import argv
from glob    import glob
from os.path import basename, join, splitext, realpath

def template(infasta, outdir):
    base, ext = splitext(basename(infasta))
    prefix = join(outdir, base + '_')
    return (prefix, ext)

def split_fasta(infasta, prefix, suffix):
    # Based on https://www.biostars.org/p/105388/#105399
    index = 0
    f=open(infasta)
    opened = False # refers to current outfile
    for line in f :
        if(line[0] == ">") :
            if(opened):
                of.close()
            opened = True
            index += 1
            outfile = prefix + str(index) + suffix
            print(outfile)
            of=open(outfile, "w")
            print(line[1:].rstrip())
        of.write(line)
    of.close()

def main(outdir, infasta):
    prefix, suffix = template(infasta, outdir)
    split_fasta(infasta, prefix, suffix)

if __name__ == '__main__':
    main(*argv[1:])
