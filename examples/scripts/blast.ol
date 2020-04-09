# load a genome in genbank format and convert to fasta
# you would want gbk_to_faa, but we use fna to demonstrate blastx
my_species = gbk_to_fna "cds" (load_gbk "examples/sequences/PCC_6803_genome.gbff")

# download the SwissProt datbase
swissprot = blastdbget_faa "swissprot"

# BLAST your genes against it
blast_hits = blastx_db 1e-20 my_species swissprot

# get a list of genes that have good hits (evalue <= 1e-20)
genes_with_hits = extract_queries blast_hits

# get a list of sequences in the database that matched those genes
nr_seq_matches = extract_targets blast_hits

result = "nothing so far"
