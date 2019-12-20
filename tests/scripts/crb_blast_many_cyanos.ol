mgen5 = load_fna "examples/sequences/Mycoplasma_genitalium_M2321_5genes.fna"
mycoplasma = load_faa_each (glob_files "examples/sequences/Mycoplasma_*_small.faa")
hitlists = crb_blast_each mgen5 mycoplasma
result = all (extract_queries_each hitlists)
