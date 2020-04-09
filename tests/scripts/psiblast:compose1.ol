query = load_faa "examples/sequences/Mycoplasma_genitalium_single.faa"
mgen = load_faa "examples/sequences/Mycoplasma_genitalium_protein_refseq.faa"
maga = load_faa "examples/sequences/Mycoplasma_agalactiae_small.faa"
mgendb = makeblastdb_faa mgen
magadb = makeblastdb_faa maga
pssms = psiblast_train_db_each 1.0e-2 query [magadb, mgendb]
table1 = psiblast_pssms_db 1.0e-2 pssms magadb
table2 = psiblast_pssms_all 1.0e-2 pssms maga
result = concat_bht [table1, table2]
