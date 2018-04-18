#!/usr/bin/env python2

# Split one multifasta file into multiple single-sequences ones.
# Names sequences sequentially to avoid any quoting issues.
# Remember ShortCut reads paths from stdout, so no printing random stuff.
# Usage: split_fasta.py <outdir> <inprefix> <infasta>

from sys     import argv
from os      import rename, remove
from os.path import join, splitext, realpath, exists
from hashlib import md5

def place_by_hash(tmpfile, outdir, md5sum, suffix):
    md5hash = str(md5sum.hexdigest())[:11] # TODO make longer?
    outfile = join(outdir, md5hash + suffix)
    if exists(outfile):
        remove(tmpfile)
    else:
        #print "%s -> %s" % (tmpfile, outfile)
        rename(tmpfile, outfile)
    print outfile # goes to output list in shortcut

def split_fasta(outdir, infasta, prefix, suffix):
    # Based on:
    #   https://www.biostars.org/p/105388/#105399
    #   https://stackoverflow.com/a/3431838

    index = 0 # tmpfile prefix (remove if only one at a time?)
    opened = False # refers to current tmpfile
    md5sum = md5() # will be updated line by line

    f=open(infasta)
    for line in f :
        # runs at the beginning of each seq
        if(line[0] == ">") :

            # skips the first seq (no outfile yet)
            if(opened):
                # note this is done once at the end too
                of.close()
                place_by_hash(tmpfile, outdir, md5sum, suffix)
                md5sum = md5()

            # update variables
            index += 1
            tmpfile = prefix + str(index) + suffix
            opened = True
            of=open(tmpfile, "w")

        # write a line to file + add it to hash
        of.write(line)
        md5sum.update(bytes(line))

    # have to repeat this to do the last one
    of.close()
    place_by_hash(tmpfile, outdir, md5sum, suffix)

def main(outdir, prefix, infasta):
    ext = splitext(infasta)[-1]
    split_fasta(outdir, infasta, prefix, ext)

if __name__ == '__main__':
    main(*argv[1:])
