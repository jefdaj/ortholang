mgen = load_faa "examples/sequences/Mycoplasma_genitalium_single.faa"
maga = load_faa "examples/sequences/Mycoplasma_agalactiae_small.faa"
hits = blastp 0.1 mgen maga
hits_faa = extract_seqs maga (extract_targets hits)
hits_aln = muscle hits_faa
result = hits_aln
