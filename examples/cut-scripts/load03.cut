# TODO what's the bug here? maybe the cds part?

# wait, what was this part even for? just to demonstrate loading a list i guess?
genome_names = load_list "examples/genome-lists/genbank-two-cyanos.txt"
genomes = load_gbk_each genome_names

# TODO bug loading the commas here on the tutorial:
pcc7942genes = concat_faa (gbk_to_faa_each "cds" (load_gbk_each
                 ["examples/sequences/PCC_7942_chr.gbk",
                  "examples/sequences/PCC_7942_pANL.gbk"]))

genes_of_interest = load_list "examples/gene-lists/pcc7942-random-100.txt"

# TODO why warning: no ID found... for all these?
sequences_of_interest = extract_seqs pcc7942genes genes_of_interest

result = sequences_of_interest
