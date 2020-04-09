single = load_faa "examples/sequences/Mycoplasma_genitalium_single.faa"
genes5 = load_faa "examples/sequences/Mycoplasma_genitalium_M2321_5genes.faa"
dbs = makeblastdb_faa_each [single, genes5]
result = dbs
