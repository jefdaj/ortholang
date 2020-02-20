maga = load_faa "examples/sequences/Mycoplasma_agalactiae_small.faa"
mgen = load_faa "examples/sequences/Mycoplasma_genitalium_protein_refseq.faa"
mgendb = mmseqs_createdb mgen
hits = mmseqs_search_db 1.0e-30 maga mgendb
result = length hits
