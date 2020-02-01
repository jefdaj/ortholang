# list everything in a folder
all_faa = glob_files "examples/sequences/*.faa"
all_fna = glob_files "examples/sequences/*.fna"

# list chromosomes from one species
pcc7942 = glob_files "examples/sequences/PCC_7942_*.gbk"

# load an existing list
two_cyanos = load_list "examples/genome-lists/genbank-two-cyanos.txt"

# use a list to load other files
two_genbank_files = load_gbk_each two_cyanos

result = pcc7942
