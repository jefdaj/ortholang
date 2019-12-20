mgen = load_faa "examples/sequences/Mycoplasma_genitalium_small.faa"
maga = gbk_to_fna "cds" (load_gbk "examples/sequences/Mycoplasma_agalactiae_PG2.gbk")
result = diamond_blastx 1.0e-50 maga mgen
