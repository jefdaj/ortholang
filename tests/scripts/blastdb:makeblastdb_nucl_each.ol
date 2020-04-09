single = load_fna "examples/sequences/Mycoplasma_genitalium_M2321_single.fna"
genes5 = load_fna "examples/sequences/Mycoplasma_genitalium_M2321_5genes.fna"
dbs = makeblastdb_fna_each [single, genes5]
result = dbs
