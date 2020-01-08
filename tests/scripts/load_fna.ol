single = load_fna "examples/sequences/Mycoplasma_genitalium_M2321_single.fna"
mapped = load_fna_each ["examples/sequences/Mycoplasma_genitalium_M2321_single.fna"]
result = [single] | mapped
