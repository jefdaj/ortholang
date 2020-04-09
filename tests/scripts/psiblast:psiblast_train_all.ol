mgen = load_faa "examples/sequences/Mycoplasma_genitalium_protein_refseq.faa"
maga = load_faa "examples/sequences/Mycoplasma_agalactiae_small.faa"
cyanodb = makeblastdb_faa_all [mgen, maga]
query = load_faa "examples/sequences/Mycoplasma_genitalium_single.faa"
pssm = psiblast_train_all 1.0e-10 query [mgen, maga]
result = pssm
