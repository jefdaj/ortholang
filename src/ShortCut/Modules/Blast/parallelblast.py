#!/usr/bin/env python

'''
Parallel BLAST uses GNU Parallel to speed up BLAST+, as suggested here:
https://www.biostars.org/p/63816/

It was adapted from an earlier "blast batch" script; I plan to remove most of
the custom options here and just take regular BLAST commands eventually. Note
that the query sequence gets split into 100K sections, so nothing much changes
if it has fewer lines than that.

Usage:
    parallelblast --help
    parallelblast -t <tmpdir> -o <outcsv> -c <cmd> -q <query> -s <subject> [-e <evalue>] [-pv]

Options:
  -h --help               Show this help text
  -c --cmd <cmd>          BLAST comand to run (blastn, tblastn, ...)
  -q --query <query>      FASTA query sequences (must match the command!)
  -s --subject <subject>  FASTA subject/target sequences (must match the command!) 
  -o --outcsv <outcsv>    Where to write the final hit table (CSV spreadsheet)
  -t --tmpdir <tmpdir>    Where to put databases and hit tables [default: bbtmp]
  -e --evalue <evalue>    E-value cutoff [default: 1e-5]
  -p --parallel           Split up query and run in parallel [default: false]
  -v --verbose            Print debugging messages
'''

from distutils.dir_util import mkpath
from docopt             import docopt
from glob               import glob
from os                 import remove, devnull
from os.path            import exists, join, basename, dirname, splitext
from subprocess         import check_call

def log(args, message):
    if args['--verbose']:
        print message

def hash_file(filename):
    with open(filename, 'r') as f:
        return str(abs(hash(f.read())))

def db_type(cmd):
    dbt = 'nucl'
    if cmd in ('blastp', 'blastx', 'tblastx'): # TODO fix these!
        dbt = 'prot'
    return dbt

def find_db(tmpdir, fasta, dbtype):
    '''
    takes the path to a fasta file
    returns the path to the corresponding blast db, which may not exist
    '''
    name = '_'.join((hash_file(fasta), dbtype))
    return join(tmpdir, name)

def make_db(args, tmpdir, fasta, dbtype):
    '''
    takes the path to a fasta file
    makes the corresponding blast db if needed
    returns the path to the db
    '''
    db = find_db(tmpdir, fasta, dbtype)
    if glob(db + '*'):
        return db
    cmd = \
        [ 'makeblastdb'
        , '-in', fasta
        , '-out', db
        , '-title', basename(db)
        , '-dbtype', dbtype
        , '-parse_seqids' # TODO sanitize those
        ]
    try:
        log(args, ' '.join(cmd))
        check_call(cmd, stdout=open(devnull, 'w'))
        return db
    except:
        for f in glob(db + '*'):
            remove(f)
        raise

def make_hits(args, db):
    '''
    takes the path to a fasta query file and a blast db
    makes the corresponding hit table if needed
    returns the path to the hit table
    '''
    query = args['--query']
    hits  = args['--outcsv']
    if exists(hits):
        return hits
    if not exists(dirname(hits)):
        mkpath(dirname(hits))
    if args['--parallel']:
        q = '-'
    else:
        q = query
    cmd = \
        [ 'BLASTDB=%s' % dirname(db)
        , args['--cmd']
        , '-db', basename(db)
        , '-query', q
        , '-evalue', args['--evalue']
        , '-outfmt 6' # output a csv; see save_header for column names
        ]
    cmd = ' '.join(cmd)
    if args['--parallel']:
        # TODO use a smaller block size? probably!
        p = "parallel --no-notice --block 100k --recstart '>' --pipe"
        cmd = "cat '%s' | %s '%s' > '%s'" % (query, p, cmd, hits)
    try:
        # log(args, '%s > %s' % (cmd, hits))
        log(args, cmd)
        with open(hits, 'w') as out:
            check_call(cmd, shell=True, stdout=out)
        return hits
    except:
        if exists(hits):
            remove(hits)
        raise

def main():
    args = docopt(__doc__, version='parallelblast 0.1')
    log(args, args)
    tmp = args['--tmpdir']
    if not exists(tmp):
        mkpath(tmp)
    dbt = db_type(args['--cmd'])
    db  = make_db(args, tmp, args['--subject'], dbt)
    out = make_hits(args, db)
    return out

if __name__ == '__main__':
    main()
