maga = load_faa "examples/sequences/Mycoplasma_agalactiae_protein_refseq.faa"
mgen = load_faa "examples/sequences/Mycoplasma_genitalium_protein_refseq.faa"
single = extract_queries (blastp 1.0e-5 maga mgen)
mapped = extract_queries_each (blastp_each 1.0e-5 maga [mgen])
result = length_each ([single] | mapped)
