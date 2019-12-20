reference = load_fna "examples/sequences/Mycoplasma_genitalium_M2321_single.fna"
small = load_faa_each ["examples/sequences/Mycoplasma_genitalium_M2321_5genes.faa"]
large = load_faa_each ["examples/sequences/Mycoplasma_agalactiae_small.faa"] |
        small
small_hits = extract_queries_each (crb_blast_each reference small)
large_hits = extract_queries_each (crb_blast_each reference large)
result = length (any large_hits ~ any small_hits)
