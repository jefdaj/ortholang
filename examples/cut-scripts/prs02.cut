pcc6803 = load_faa "examples/sequences/PCC_6803_genes.faa"
pcc7942 = concat_faa [gbk_to_faa "cds" (load_gbk "examples/sequences/PCC_7942_chr.gbk"),
                      gbk_to_faa "cds" (load_gbk "examples/sequences/PCC_7942_pANL.gbk")]

# BLAST Synechococcus genes against Synechocystis with a standard cutoff
cutoff = 1e-10
hits = extract_queries (blastp cutoff pcc7942 pcc6803)

# Re-run it with a range of cutoffs and report the number of hits for each
cutoffs = [1e-5, 1e-10, 1e-20, 1e-50, 1e-100]
lengths = replace_each (length hits) cutoff cutoffs
result = lengths
