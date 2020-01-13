maga5 = load_faa "examples/sequences/Mycoplasma_genitalium_M2321_5genes.faa"
maga = load_fna "examples/sequences/Mycoplasma_genitalium_M2321_5genes.fna"
single = extract_queries (tblastn 1.0e-5 maga5 maga)
mapped = extract_queries_each (tblastn_each 1.0e-5 maga5 [maga])
result = all ([single] | mapped)
