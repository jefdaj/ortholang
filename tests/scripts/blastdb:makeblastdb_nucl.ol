single = load_fna "examples/sequences/Mycoplasma_genitalium_M2321_single.fna"
genes5 = load_fna "examples/sequences/Mycoplasma_genitalium_M2321_5genes.fna"
singles = [makeblastdb_fna single, makeblastdb_fna genes5]
each = makeblastdb_fna_each [single, genes5]
both = makeblastdb_fna_all [single, genes5]
result = length_each [singles, each, singles | each, [both]]
