maga = load_faa "examples/sequences/Mycoplasma_agalactiae_small.faa"
query = load_faa "examples/sequences/Mycoplasma_genitalium_single.faa"
pssm = psiblast_train 1.0e-10 query maga
result = pssm