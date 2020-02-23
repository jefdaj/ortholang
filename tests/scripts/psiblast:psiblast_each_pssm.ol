query   = load_faa "examples/sequences/Mycoplasma_genitalium_single.faa"
mgen = load_faa "examples/sequences/Mycoplasma_genitalium_protein_refseq.faa"
maga = load_faa "examples/sequences/Mycoplasma_agalactiae_small.faa"
pssms   = psiblast_train_each 0.1 query [mgen, maga]
# works:
# result = pssms
# fails:
tables  = psiblast_each_pssm  0.1 pssms mgen # TODO this fails :(
result  = tables
