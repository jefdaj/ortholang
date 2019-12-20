query = load_faa "examples/sequences/Mycoplasma_genitalium_single.faa"
mgen = load_faa "examples/sequences/Mycoplasma_genitalium_protein_refseq.faa"
maga = load_faa "examples/sequences/Mycoplasma_agalactiae_small.faa"
cyanodb = makeblastdb_prot_all [mgen, maga]
pssm = psiblast_train_db 1.0e-2 query cyanodb
hitlists = psiblast_pssm_each 1.0e-2 pssm [mgen, maga]
result = hitlists
