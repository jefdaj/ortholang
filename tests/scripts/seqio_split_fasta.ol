multifa = load_faa "examples/sequences/Mycoplasma_genitalium_M2321_5genes.faa"
genes = split_faa multifa
mapped = split_faa_each [multifa]
result = length_each ([genes] | mapped)
