fna = load_fna "examples/sequences/Mycoplasma_genitalium_M2321_5genes.fna"
faa = load_faa "examples/sequences/Mycoplasma_genitalium_M2321_5genes.faa"
singleFna = extract_ids fna
singleFaa = extract_ids faa
mappedFna = extract_ids_each [fna]
mappedFaa = extract_ids_each [faa]
bothFna = [singleFna] | mappedFna
bothFaa = [singleFaa] | mappedFaa
result = bothFna | bothFaa
