#!/usr/bin/env python

# TODO add genome name to blast hits so you can tell which to delete?

'''
The bblast ("batch blast") script simplifies doing large numbers of local BLAST
searches. Just pass a FASTA file with your query sequences, a blast command,
and a list of FASTA files to search against. bblast will make the necessary
databases, run the searches, and return a spreadsheet of all the hits.
Intermediate results are cached, so running variations on the same search is
very fast.

For now each FASTA file is used to make a separate database. If you want to
make one big database from all of them instead, bug Jeff to add that! It
changes how you interpret the e-values.

Usage:
  bblast --help
  bblast -o <outcsv> -d <dblist> -f <fasta> -c <cmd> [-t <tmpdir>] [-v]

Options:
  -h --help              Show this help text
  -c --blast-cmd  <cmd>  BLAST comand to run (blastn, tblastn, ...)
  -d --db-list <dblist>  File containing a list of FASTA files to search against
  -f --fasta    <fasta>  FASTA query sequences (must match the command!)
  -o --out-csv <outcsv>  Where to write the final hit table (CSV spreadsheet)
  -t --tmp-dir <tmpdir>  Where to put databases and hit tables [default: bbtmp]
  -v --verbose           Print debugging messages
'''

from datetime           import datetime
from distutils.dir_util import mkpath
from docopt             import docopt
from glob               import glob
from multiprocessing    import Pool
from os                 import remove, devnull
from os.path            import exists, join, basename, dirname, splitext
from subprocess         import check_call, PIPE, Popen

#############
# utilities #
#############

def log(args, message, level=1):
    if args['--verbose'] or level > 1:
        print message

def setup(args):
    log(args, args)
    tmp = args['--tmp-dir']
    dbs  = join(tmp, 'blast_db')
    hits = join(tmp, 'blast_hits')
    for d in [dbs, hits]:
        if not exists(d):
            mkpath(d)

def read_list(filename):
    with open(filename, 'r') as f:
        return [l.strip() for l in f.readlines()]

def hash_file(filename):
    with open(filename, 'r') as f:
        return str(abs(hash(f.read())))

##########################
# locate hashed tmpfiles #
##########################

def find_db(args, fasta):
    '''
    takes the path to a fasta file
    returns the path to the corresponding blast db, which may not exist
    '''
    tmpd = join(args['--tmp-dir'], 'blast_db')
    base = hash_file(fasta)
    return join(tmpd, base, base)

def find_hits(args, db):
    '''
    takes paths to a query file + database, and a blast command
    returns the path to the corresponding hits table, which may not exist
    '''
    tmpd = join(args['--tmp-dir'], 'blast_hits', hash_file(args['<fasta>']))
    return join(tmpd, '%s.csv' % basename(db))

######################
# run blast commands #
######################

def make_db(args, dbfasta):
    '''
    takes the path to a fasta file
    makes the corresponding blast db if needed
    returns the path to the db
    '''
    db = find_db(args, dbfasta)
    if glob(db + '*'):
        return db
    # TODO make this adapt to the command given!
    dbt = 'nucl'
    if args['<cmd>'] == 'blastp':
        dbt = 'prot'
    cmd = \
        [ 'makeblastdb'
        , '-in', dbfasta
        , '-out', db
        , '-title', basename(db)
        , '-dbtype', dbt
        ]
    try:
        log(args, ' '.join(cmd))
        check_call(cmd, stdout=open(devnull, 'w'))
        return db
    except:
        for f in glob(db):
            remove(f)
        raise

def make_hits(args, dbfasta, db):
    '''
    takes the path to a fasta query file and a blast db, plus the db itself
    makes the corresponding hit table if needed
    returns the path to the hit table
    '''
    query = args['<fasta>']
    hits = find_hits(args, db)
    if not exists(dirname(hits)):
        mkpath(dirname(hits))
    if exists(hits):
        return hits
    cmd = \
        [ 'BLASTDB=%s' % dirname(db)
        , args['<cmd>']
        , '-db', basename(db)
        , '-query', '-'
        , '-outfmt 10' # output a csv; see save_header for column names
        ]
    cmd = ' '.join(cmd)
    # BLAST+ doesn't parallelize well on its own, so this uses GNU parallel
    # to break the query file into chunks and run them at the same time.
    # See these Biostar threads:
    #   https://www.biostars.org/p/63816/
    #   https://www.biostars.org/p/76009/
    # TODO time with different block sizes, or look up optimal
    p = "parallel --no-notice -N 100 --recstart '>' --pipe"
    cmd = "cat '%s' | %s '%s'" % (query, p, cmd)
    try:
        log(args, cmd)
        with open(hits, 'w') as out:
            # For some reason this is the idiom required to process lines from
            # stdout in real time. `for line in stdout` waits until the script
            # has finished.
            proc = Popen(cmd, shell=True, stdout=PIPE)
            while True:
                line = proc.stdout.readline()
                if not line:
                    break
                out.write(','.join((dbfasta, line)))
        return hits
    except:
        if exists(hits):
            remove(hits)
        raise

###################################
# write one big table of all hits #
###################################

def header(args):
    h = 'dbname,queryid,subjectid,percentidentity,alignmentlength' \
      + ',mismatches,gapopens,qstart,qend,sstart,send,evalue,bitscore\n'
    return h

def save_hits(args, hitpaths):
    # TODO are these different for different blast commands??
    #      (maybe not, if the -outfmt 10 above always holds?)
    outpath = args['--out-csv']
    try:
        with open(outpath, 'w') as out:
            out.write(header(args))
            for path in hitpaths:
                with open(path, 'r') as hits:
                    for line in hits.readlines():
                        out.write(line)
    except:
        if exists(outpath):
            remove(outpath)
        raise

########
# main #
########

def main():
    args = docopt(__doc__, version='bblast 0.1')
    setup(args)
    dbfastas = read_list(args['--db-list'])
    hits = []; ncur = 0; ntot = len(dbfastas)

    # TODO replace with the block below, if you can fix that
    for dbfasta in dbfastas:
        ncur += 1
        db = make_db(args, dbfasta)
        if not exists(find_hits(args, db)):
            date = datetime.now().strftime('%H:%M:%S')
            msg = '# %s %s %s/%s: %s' % (date, args['<cmd>'], ncur, ntot, dbfasta)
            log(args, msg, level=2)
        new = make_hits(args, dbfasta, db)
        hits.append(new)

    # TODO this is faster for small genomes, but causes a problem with parallel:
    # "unable to close filehandle properly: Broken pipe during global destruction."
    # pool = Pool()
    # for dbfasta in dbfastas:
    #     ncur += 1
    #     db = make_db(args, dbfasta)
    #     if not exists(find_hits(args, db)):
    #         date = datetime.now().strftime('%H:%M:%S')
    #         msg = '# %s %s %s/%s: %s' % (date, args['<cmd>'], ncur, ntot, dbfasta)
    #         log(args, msg, level=2)
    #     pool.apply_async(make_hits, (args, dbfasta, db), callback=hits.append)
    # pool.close()

    save_hits(args, hits)

if __name__ == '__main__':
    main()
