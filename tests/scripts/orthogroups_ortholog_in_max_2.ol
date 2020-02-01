smallbacteria = load_faa_each ["examples/sequences/Mycoplasma_agalactiae_small.faa", "examples/sequences/Mycoplasma_genitalium_small.faa"]
ofres = orthofinder smallbacteria
ofogs = orthogroups ofres
result = length (ortholog_in_max 2 ofres smallbacteria)
