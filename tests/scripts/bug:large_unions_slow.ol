qfna = load_fna "examples/sequences/Mycoplasma_genitalium_M2321_5genes.fna"
qfaa = load_faa "examples/sequences/Mycoplasma_genitalium_M2321_5genes.faa"
sfna1 = load_fna "examples/sequences/Mycoplasma_genitalium_M2321_5genes.fna"
sfaa1 = load_faa "examples/sequences/Mycoplasma_agalactiae_small.faa"
sfna2 = load_fna "examples/sequences/Mycoplasma_genitalium_M2321_5genes.fna"
sfaa2 = load_faa "examples/sequences/Mycoplasma_agalactiae_small.faa"
sdbfna = [blastn_db 1.0e-5 qfna (makeblastdb_nucl sfna1), blastn_db 1.0e-5 qfna (makeblastdb_nucl sfna2)]
sdbfaa = [blastp_db 1.0e-5 qfaa (makeblastdb_prot sfaa1), blastp_db 1.0e-5 qfaa (makeblastdb_prot sfaa2)]
sfafaa = [blastp 1.0e-5 qfaa sfaa1, blastp 1.0e-5 qfaa sfaa2]
mdbfna = blastn_db_each 1.0e-5 qfna [makeblastdb_nucl sfna1, makeblastdb_nucl sfna2]
mfafaa = blastp_each 1.0e-5 qfaa [sfaa1, sfaa2]
result = any (extract_queries_each sdbfna | extract_queries_each sdbfaa | extract_queries_each mdbfna |
              extract_queries_each sfafaa |
              extract_queries_each mfafaa)