maga = load_faa "examples/sequences/Mycoplasma_agalactiae_small.faa"
mgen = load_faa "examples/sequences/Mycoplasma_genitalium_protein_refseq.faa"
hits = mmseqs_search 1.0e-30 maga mgen
result = length hits
