# TODO add a couple more?
smallbacteria = load_faa_each ["examples/sequences/Mycoplasma_agalactiae_small.faa",
                               "examples/sequences/Mycoplasma_genitalium_small.faa"]
ofres = orthofinder smallbacteria
result = ofres
