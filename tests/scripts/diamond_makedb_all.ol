mycoplasma = load_faa_each (glob_files "examples/sequences/Mycoplasma_*_small.faa")
result = diamond_makedb_all mycoplasma
