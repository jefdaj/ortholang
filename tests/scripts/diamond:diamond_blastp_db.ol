mgen = load_faa "examples/sequences/Mycoplasma_genitalium_small.faa"
maga = load_faa "examples/sequences/Mycoplasma_agalactiae_small.faa"
db = diamond_makedb maga
hits = diamond_blastp_db 0 1.0e-20 mgen db
result = hits
