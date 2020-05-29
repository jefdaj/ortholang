query = load_faa "examples/sequences/Mycoplasma_genitalium_single.faa"
mgendb = makeblastdb_prot (load_faa "examples/sequences/Mycoplasma_genitalium_protein_refseq.faa")
magadb = makeblastdb_prot (load_faa "examples/sequences/Mycoplasma_agalactiae_small.faa")
pssm = psiblast_train_db_each 1.0e-2 query [mgendb, magadb]
result = pssm
