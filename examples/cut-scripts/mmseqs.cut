# TODO explain why the two methods come up witi different hits

# list M. agalactiae genes with good hits in M. bovis
maga   = load_faa "examples/sequences/Mycoplasma_agalactiae_small.faa"
mbov   = load_faa "examples/sequences/Mycoplasma_bovis_small.fa"
hits1  = mmseqs_search 1.0e-20 maga mbov
genes1 = extract_queries hits1

# list M. agalactiae genes with good hits in every Mycoplasma species
# the [maga] part subtracts maga from the others list
others = load_faa_each (glob_files "examples/sequences/Mycoplasma_*_refseq.faa") ~ [maga]
hits2  = mmseqs_search_db 1e-20 maga (mmseqs_createdb_all others)
genes2 = extract_queries hits2

# list genes found in maga and mbov but not one of the others
result = genes1 ~ genes2
