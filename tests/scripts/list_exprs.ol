# result should be 1 (all lists are the same)

list1 = load_faa_each ["examples/sequences/Mycoplasma_agalactiae_small.faa"]
list2 = [load_faa "examples/sequences/Mycoplasma_agalactiae_small.faa"]
list3 = [load_faa "examples/sequences/Mycoplasma_agalactiae_small.faa"]
list4 = [load_faa "examples/sequences/Mycoplasma_agalactiae_small.faa"]
result = length (list1 | list2 | list3 | list4)
