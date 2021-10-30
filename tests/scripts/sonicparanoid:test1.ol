mycoplasma = load_faa_glob "examples/sequences/Mycoplasma*.faa"
n_genomes = 8

# TODO don't return just the pairs by default, but have an extractor fn for it
pairs = sonicparanoid (sample n_genomes mycoplasma)
result = pairs
