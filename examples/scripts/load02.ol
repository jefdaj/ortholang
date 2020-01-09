# load a genome distributed as separate genbank files,
# and convert it to one FASTA
pcc7942v1 = concat_faa [gbk_to_faa "cds" (load_gbk "examples/sequences/PCC_7942_chr.gbk"),
                        gbk_to_faa "cds" (load_gbk "examples/sequences/PCC_7942_pANL.gbk")]

# same, but with globbing
# TODO bug loading this line in the tutorial?
pcc7942v2 = concat_faa (gbk_to_faa_each "cds" (load_gbk_glob "examples/sequences/PCC_7942_*.gbk"))

result = pcc7942v2
