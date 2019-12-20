query = load_faa "examples/sequences/Mycoplasma_genitalium_single.faa"
mgen = load_faa "examples/sequences/Mycoplasma_genitalium_G37_protein_refseq.faa"
hits = blastp 0.1 query mgen
hits_faa = extract_seqs mgen (extract_targets hits)
hits_aln = muscle hits_faa
hits_hmm = hmmbuild hits_aln
result = hits_hmm
