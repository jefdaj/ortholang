#!/usr/bin/env python2

# Split one multifasta file into multiple single-sequences ones.
# Names sequences sequentially to avoid any quoting issues.
# Usage: split_fasta.py <outdir> <inprefix> <infasta>

from sys     import argv
from os      import rename, remove, makedirs
from os.path import join, splitext, realpath, exists, dirname
from hashlib import md5

def break_up_hash(md5hash):
    # break into a hierarchy of subdirs to speed file access
    # TODO generalize this in Haskell for other dirs too if needed
    return md5hash

def split_len(seq, length):
    # http://code.activestate.com/recipes/496784-split-string-into-n-size-pieces/
    return [seq[i:i+length] for i in range(0, len(seq), length)]

def place_by_hash(outhandle, tmpfile, outdir, md5sum, suffix):
    md5hash = str(md5sum.hexdigest())[:11] # TODO make longer?
    outfile = join(outdir, join(*split_len(md5hash, 2)) + suffix)
    outdir  = dirname(outfile)
    try:
        makedirs(outdir)
    except OSError:
        pass
    if exists(outfile):
        remove(tmpfile)
    else:
        #print "%s -> %s" % (tmpfile, outfile)
        rename(tmpfile, outfile)
    # print outfile # goes to output list in ortholang
    outhandle.write(outfile + '\n')

def split_fasta(outlist, outdir, infasta, prefix, suffix):
    # Based on:
    #   https://www.biostars.org/p/105388/#105399
    #   https://stackoverflow.com/a/3431838

    try:
        makedirs(prefix)
    except OSError:
        pass

    index = 0 # tmpfile prefix (remove if only one at a time?)
    opened = False # refers to current tmpfile
    md5sum = md5() # will be updated line by line

    f=open(infasta)
    with open(outlist, 'w') as oh:
        for line in f :
            # runs at the beginning of each seq
            if(line[0] == ">") :
    
                # skips the first seq (no outfile yet)
                if(opened):
                    # note this is done once at the end too
                    of.close()
                    place_by_hash(oh, tmpfile, outdir, md5sum, suffix)
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
        place_by_hash(oh, tmpfile, outdir, md5sum, suffix)

def main(outlist, outdir, prefix, infasta, ext):
    suffix = '.' + ext
    split_fasta(outlist, outdir, infasta, prefix, suffix)

if __name__ == '__main__':
    main(*argv[1:])
