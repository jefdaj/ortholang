query = load_faa "examples/sequences/Mycoplasma_genitalium_single.faa"
mgen = makeblastdb_faa (load_faa "examples/sequences/Mycoplasma_agalactiae_small.faa")
maga = makeblastdb_faa (load_faa "examples/sequences/Mycoplasma_genitalium_small.faa")
hitlists = psiblast_db_each 1.0e-2 query [mgen, maga]
result = concat_bht hitlists
