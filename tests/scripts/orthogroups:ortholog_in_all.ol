mycoplasma = load_faa_glob "examples/sequences/Mycoplasma_*_refseq.faa"
ofres = orthofinder (sample 3 mycoplasma)
shared = ortholog_in_all ofres mycoplasma # nondeterministic?
result = shared
