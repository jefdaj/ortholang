list1 = load_faa_each ["examples/sequences/Mycoplasma_agalactiae_small.faa"]
list2 = [load_faa "examples/sequences/Mycoplasma_agalactiae_small.faa"]
result = length (list1 | list2)
