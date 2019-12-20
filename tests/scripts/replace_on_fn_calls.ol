faa_paths = ["examples/sequences/Mycoplasma_agalactiae_PG2.gbk",
             "examples/sequences/Mycoplasma_arthritidis_protein_refseq.faa",
             "examples/sequences/Mycoplasma_capricolum_protein_refseq.faa",
             "examples/sequences/Mycoplasma_genitalium_M2321_5genes.fna"]
faa_path = "examples/sequences/Mycoplasma_agalactiae_PG2_protein_refseq.faa"
faa = load_faa faa_path
faa_alternatives = replace_each faa faa_path faa_paths
faa_paths_fn = glob_files "examples/sequences/Mycoplasma_*.faa"
result = faa_alternatives
