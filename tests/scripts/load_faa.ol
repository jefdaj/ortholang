single = load_faa "examples/sequences/Mycoplasma_genitalium_single.faa"
mapped = load_faa_each ["examples/sequences/Mycoplasma_genitalium_single.faa"]
result = [single] | mapped
