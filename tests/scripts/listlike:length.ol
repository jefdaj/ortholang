# Tests that we can get the length of various "list-like" things.
# TODO allow heterogenous lists? no. probably more complication than it's worth

mgen5 = load_fna "examples/sequences/Mycoplasma_genitalium_M2321_5genes.fna"
mbov  = load_faa "examples/sequences/Mycoplasma_bovis_small.fa"

e = 1e-20
hits1 = blastx e mgen5 mbov
# hits2 = crb_blast mgen5 mbov TODO fix this
hits3 = mmseqs_search e mgen5 mbov

# TODO lazy list eval! getting length of this shouldn't require any blast operations
actualList = [hits1, hits3]

result =
 [length hits1,
  length hits3,
  length actualList]
