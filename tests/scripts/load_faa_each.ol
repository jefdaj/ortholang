list1 = load_faa_each ["examples/sequences/Mycoplasma_genitalium_single.faa"]
list2 = [load_faa "examples/sequences/Mycoplasma_genitalium_single.faa"]
result = length (list1 | list2)
