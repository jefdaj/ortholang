# ShortCut mainly deals with files you load yourself, but there are also a few
# functions for downloading things from public databases.

# You can list BLAST databases available from NCBI like this:
ncbi_db_list = blastdblist ""
ncbi_human_db_list = blastdblist "human"

# And then download them like this (WARNING: huge files! 100GB+)
# Annoyingly, you have to figure out yourself whether each database is protein
# or nucleic acid. But in most cases you probably already know.
human_est = blastdbget_prot "human_est"
swissprot = blastdbget_prot "swissprot"
rsprotein = blastdbget_prot "refseq_protein"
rsgenomic = blastdbget_nucl "refseq_genomic"

# For genomes and proteomes, try BioMartR.
# Give the complete species name and the database in parentheses.
# It often works the first time, but if not you'll need to look up the name first.
pcc7942p = get_proteomes ["Synechococcus elongatus PCC 7942 (refseq)"]
pcc7942n = get_genomes   ["Synechococcus elongatus PCC 7942 (refseq)"]
pcc6803  = get_proteomes ["Synechocystis sp. PCC 6803 (refseq)"]

# TODO what should we evaluate here?
result = ncbi_human_db_list
