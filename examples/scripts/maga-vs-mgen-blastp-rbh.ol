# first we load a protein fasta file (fasta amino acid) for each proteome
maga = load_faa "examples/sequences/Mycoplasma_agalactiae_small.faa"
mgen = load_faa "examples/sequences/Mycoplasma_genitalium_small.faa"

# blast each one against the other with a relatively lax cutoff
# (if you have something besides two protein fastas, use different blast functions here)
fwdHits = blastp 1e-5 maga mgen
revHits = blastp 1e-5 mgen maga

# keep only the pairs of genes where each one is the other's best hit
recHits = reciprocal_best fwdHits revHits

result = recHits
