query = load_faa "examples/sequences/Mycoplasma_genitalium_single.faa"
maga = load_faa "examples/sequences/Mycoplasma_agalactiae_small.faa"
magadb = makeblastdb_prot maga
pssm = psiblast_train_db 1.0e-2 query magadb
hittable = psiblast_pssm_db 1.0e-2 pssm magadb
result = hittable
