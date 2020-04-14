# more complicated load functions useful in different situations.
# each loads two files and converts them to one proteome

# you have multiple similar files you want to combine into one variable
pcc7942v1 = concat_faa (gbk_to_faa_each "cds" (load_gbk_each
                                                ["examples/sequences/PCC_7942_chr.gbk",
                                                 "examples/sequences/PCC_7942_pANL.gbk"]))

# you want to make one variable (file), but need to get the parts in different ways
# for example, one of these is a genbank file and the other is fasta nucleic acid
pcc7942v2 = concat_faa [translate (load_fna "examples/sequences/PCC_7942_genome.fna"),
                        gbk_to_faa "cds" (load_gbk "examples/sequences/PCC_7942_pANL.gbk")]

# you want to make one variable, but also be able to reference the parts separately
# (or you just think v2 looks overly complicated on one line)
chr  = load_fna "examples/sequences/PCC_7942_genome.fna"
panl = load_gbk "examples/sequences/PCC_7942_pANL.gbk"
pcc7942v3 = concat_faa [translate chr, gbk_to_faa "cds" panl]

# you have an existing list of multiple similar files,
# and you want to combine them into one variable
pcc7942v4 = concat_faa
              (gbk_to_faa_each "cds"
                (load_gbk_each
                    (load_list "examples/genome-lists/pcc-7942-genbank.txt")))

# you have multiple similar files and want to combine them into one variable,
# but instead of an explicit list you name the files following a wildcard ("glob") pattern
pcc7942v5 = concat_faa
              (gbk_to_faa_each "cds"
                (load_gbk_glob "examples/sequences/PCC_7942_*.gbk"))

result = [pcc7942v1, pcc7942v2, pcc7942v3, pcc7942v4, pcc7942v5]
