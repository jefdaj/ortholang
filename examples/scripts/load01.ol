# load two transcriptomes individually
pcc6803 = load_fna "examples/sequences/PCC_6803_genes.fna"
pcc7942 = load_fna "examples/sequences/PCC_7942_genes.fna"

# load both at once
# TODO and a couple otehrs right?
# TODO does {} notation work in globs?
both = load_fna_glob "examples/sequences/PCC_*_genes.fna"

result = both
