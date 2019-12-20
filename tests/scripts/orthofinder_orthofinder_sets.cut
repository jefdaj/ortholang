mycoplasma = load_faa_each ["examples/sequences/Mycoplasma_agalactiae_small.faa",
                            "examples/sequences/Mycoplasma_genitalium_small.faa"]
mycosets = leave_each_out mycoplasma
orthologs = orthofinder mycoplasma
# TODO have to rewrite replace_each before this will work :(
orthosets = replace_each orthologs mycoplasma mycosets
result = orthosets
