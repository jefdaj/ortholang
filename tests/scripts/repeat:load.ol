# This tests for a bug where repeating the same load operation caused
# zero-length .faa files. Now load_* functions are exempted from repeat.
# TODO is that still true with the Stochastic tags?

maga = load_faa "examples/sequences/Mycoplasma_agalactiae_PG2_protein_refseq.faa"
mgen = load_faa "examples/sequences/Mycoplasma_genitalium_protein_refseq.faa"
fwdHits = extract_targets (blastp 1.0e-5 maga mgen)
result = all (repeat fwdHits mgen 10)
