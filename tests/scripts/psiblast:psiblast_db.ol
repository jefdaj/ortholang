query = load_faa "examples/sequences/Mycoplasma_genitalium_single.faa"
maga = makeblastdb_faa (load_faa "examples/sequences/Mycoplasma_agalactiae_small.faa")
hits = psiblast_db 1.0e-10 query maga
result = hits
