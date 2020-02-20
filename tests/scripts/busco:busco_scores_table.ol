odb9_proteobacteria = busco_fetch_lineage "v2/datasets/proteobacteria_odb9"
mgen = load_faa "examples/sequences/Mycoplasma_genitalium_G37_protein_refseq.faa"
maga = load_faa "examples/sequences/Mycoplasma_agalactiae_PG2_protein_refseq.faa"
scores = busco_proteins_each odb9_proteobacteria [maga, mgen]
table = busco_scores_table scores
result = table
