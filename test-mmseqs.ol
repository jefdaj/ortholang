maga = load_faa "examples/sequences/Mycoplasma_agalactiae_small.faa"
mbov = load_faa "examples/sequences/Mycoplasma_bovis_small.fa"
maga_db = mmseqs_createdb_all [maga]
hits = mmseqs_search_db 1e-20 mbov maga_db
result = hits
