single = load_gbk "examples/sequences/Mycoplasma_genitalium_M2321.gbk"
mapped = load_gbk_each ["examples/sequences/Mycoplasma_genitalium_M2321.gbk"]
result = [single] | mapped
