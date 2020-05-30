qfaa = load_faa "examples/sequences/Mycoplasma_genitalium_M2321_5genes.faa"
sfaa1 = load_faa "examples/sequences/Mycoplasma_agalactiae_small.faa"
sfaa2 = load_faa "examples/sequences/Mycoplasma_agalactiae_small.faa"
sdbfaa = [blastp_db 1.0e-5 qfaa (makeblastdb_prot sfaa1), blastp_db 1.0e-5 qfaa (makeblastdb_prot sfaa2)]
result = plot_vars "bug! blastp_db nodes missing because no vars"
