mgen = load_faa "examples/sequences/Mycoplasma_genitalium_protein_refseq.faa"
maga = load_faa "examples/sequences/Mycoplasma_agalactiae_small.faa"
cyanodb = makeblastdb_prot_all [mgen, maga]
query = load_faa "examples/sequences/Mycoplasma_genitalium_single.faa"
pssm = psiblast_train_faa_pdb 1.0e-10 query cyanodb
result = pssm