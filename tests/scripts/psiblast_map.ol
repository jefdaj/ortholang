# psiblast functions that use mapNofM but no expr-editing helpers
# (this is an implementation detail, not something important to know as a user)

# setup
query   = load_faa "examples/sequences/Mycoplasma_genitalium_single.faa"
queries = split_faa (load_faa "examples/sequences/Mycoplasma_genitalium_M2321_5genes.faa")
mgen = load_faa "examples/sequences/Mycoplasma_genitalium_protein_refseq.faa"
maga = load_faa "examples/sequences/Mycoplasma_agalactiae_small.faa"
mgendb  = makeblastdb_prot mgen
magadb  = makeblastdb_prot maga
pssm    = psiblast_train_db 1e-2 query magadb

# training fns
pssms1 = psiblast_train_db_each 1e-2 query [magadb, mgendb]
pssms2 = psiblast_train_pssms_db 1e-2 queries magadb

# search fns
tables1 = psiblast_pssm_db_each 0.1 pssm [mgendb, magadb]
tables2 = psiblast_each_pssm_db 0.1 pssms1 magadb
tables3 = psiblast_each_pssm    0.1 pssms1 maga

# using length here to get a list that depends on everything
result = [length pssms1, length pssms2,
          length tables1, length tables2, length tables3]
