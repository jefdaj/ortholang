# When training a pssm fails because the e-value is too low/db too distantly
# related, OrthoLang creates an "empty" pssm to track that fact rather than
# failing. This tests that it works properly by trying searches that would fail
# otherwise.

# the (maga) query gene has an exact match in its own genome,
# so there should be a hit at any e-value
query   = load_faa "examples/sequences/Mycoplasma_genitalium_single.faa"
maga = load_faa "examples/sequences/Mycoplasma_agalactiae_small.faa"
magadb  = makeblastdb_prot maga

# there's no perfect hit in another cyano though,
# so below a certain cutoff an empty pssm will be created if trained on mgen
mgen = load_faa "examples/sequences/Mycoplasma_genitalium_protein_refseq.faa"
mgendb  = makeblastdb_prot mgen

# check that psiblast itself works by training a pssm on each
cutoff   = 0.5
pssmmaga = psiblast_train_db cutoff query magadb
pssmmgen = psiblast_train_db cutoff query mgendb

# check that it keeps working on maga with increasingly strict cutoffs
# (note: a cutoff of 0 will break this, but give results in a regular search)
cutoffs   = [1, 0.5, 0.1, 1e-2, 1e-3, 1e-5, 1e-10, 1e-20, 1e-50, 1e-100]
pssmsmaga = replace_each pssmmaga cutoff cutoffs

# try overly strict cutoffs with mgen, which would break without empty pssms
# (note: which training command actually throws the error depends on evaluation order)
pssmsmgen = replace_each pssmmgen cutoff cutoffs

# finally, try doing searches on all of them.
# the overly strict ones should have 0 hits rather than throwing an error
# TODO this fails in the makeblastdb_prot step
hitsmaga = psiblast_each_pssm cutoff pssmsmaga maga
hitsmgen = psiblast_each_pssm cutoff pssmsmgen mgen

# we only need to evaluate the mgen ones
# but if they break try the maga ones to be sure it's not a general issue
# should be [2, 2, 2, 2, 2, 0, 0, 0, 0, 0]
#result = length_each hitsmgen
result = hitsmgen
