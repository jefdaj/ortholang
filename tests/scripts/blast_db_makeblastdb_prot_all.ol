single = load_faa "examples/sequences/Mycoplasma_genitalium_single.faa"
genes5 = load_faa "examples/sequences/Mycoplasma_genitalium_M2321_5genes.faa"
db = makeblastdb_prot_all [single, genes5]
result = db
