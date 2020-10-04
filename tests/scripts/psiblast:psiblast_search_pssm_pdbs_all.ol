query = load_faa "examples/sequences/Mycoplasma_genitalium_single.faa"
mgen = load_faa "examples/sequences/Mycoplasma_genitalium_protein_refseq.faa"
maga = load_faa "examples/sequences/Mycoplasma_agalactiae_small.faa"
db1 = makeblastdb_prot mgen
db2 = makeblastdb_prot maga
pssm = psiblast_train_faa_pdb 1.0e-10 query db1
lst    = psiblast_search_pssm_pdbs     1.0e-10 pssm [db1, db2]

# TODO fix this:
single = psiblast_search_pssm_pdbs_all 1.0e-10 pssm [db1, db2]

result = lst | [single]
