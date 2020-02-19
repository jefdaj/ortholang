# TODO load_blastdb_{nucl,prot} functions!

# You can get the standard BLAST databases from NCBI,
# but be careful because they may be really big (100GB+).
# See examples/scripts/download.ol for details.
#
# If you only want to do a single search against one of these, the NCBI website
# will be much faster. But it may be convenient to fetch them locally to use in
# a larger pipeline, or because you want to do a large number of searches in
# parallel.
human     = blastdbget_prot "human_est"
swissprot = blastdbget_prot "swissprot"

# You can also make your own databases from one or more species of interest.
# Note that if you just want to do a quick BLAST search you can skip this and
# use FASTA files. The databases will be created automatically as needed. But
# you might still want to make your own to simplify doing many similar
# searches, or you might have existing databases you need to load.
#
# Here we make a database for our 4 Mycoplasma genomes.
#
# Note the types: the _all functions take a list of inputs and make one output.
# If you wanted a list of databases, one for each proteome, you would use
# makeblastdb_prot_each instead.
mycodb = makeblastdb_prot_all (load_faa_glob "examples/sequences/Mycoplasma_*_refseq.faa")

# The convention in OrthoLang is that BLAST functions expect to be passed FASTA
# files and make any needed databases themselves. But for each function that
# does that there's another with a _db suffix that takes the raw database.
# For example, the tblastn and tblastn_db calls here will come out the same:
pcc7942 = load_faa "examples/sequences/PCC_7942_genes.faa"
pcc6803 = load_fna "examples/sequences/PCC_6803_genes.fna"
hits1 = tblastn    1e-50 pcc7942  pcc6803
hits2 = tblastn_db 1e-50 pcc7942 (makeblastdb_nucl pcc6803)

# TODO what should be evaluated?
# result = [human, swissprot, mycodb, makeblastdb_prot pcc6803]
# result = [human, swissprot, mycodb]
result = "nothing so far"
