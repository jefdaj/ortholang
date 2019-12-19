pcc6803 = load_faa "examples/sequences/PCC_6803_genes.faa"
pcc7942 = concat_faa [gbk_to_faa "cds" (load_gbk "examples/sequences/PCC_7942_chr.gbk"),
                      gbk_to_faa "cds" (load_gbk "examples/sequences/PCC_7942_pANL.gbk")]

# BLAST against the SwissProt database
hits = blastp_db 1e-50 pcc7942 (blastdbget_prot "swissprot")

# find Synechococcus orthologs of Synechocystis genes
# (this lists the Synechococcus genes themselves;
#  for the Synechocystis genes that have orthologs, use extract_queries)
#genes = extract_targets (blastx 1e-20 pcc6803 pcc7942)
#
