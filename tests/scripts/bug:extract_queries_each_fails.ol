qfna = load_fna "examples/sequences/Mycoplasma_genitalium_M2321_5genes.fna"
sfna1 = load_fna "examples/sequences/Mycoplasma_genitalium_M2321_5genes.fna"
sfna2 = load_fna "examples/sequences/Mycoplasma_genitalium_M2321_5genes.fna"
mfafna = blastn_each 1.0e-5 qfna [sfna1, sfna2]
result = extract_queries_each mfafna