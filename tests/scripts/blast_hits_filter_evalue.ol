small = load_faa "examples/sequences/Mycoplasma_genitalium_small.faa"
mgen = load_faa "examples/sequences/Mycoplasma_genitalium_G37_protein_refseq.faa"
hits = blastp 1.0e-5 small mgen
single = extract_queries (filter_evalue 1.0e-50 hits)
mapped = extract_queries_each (filter_evalue_each 1.0e-50 [hits])
result = length ([single] | mapped)
