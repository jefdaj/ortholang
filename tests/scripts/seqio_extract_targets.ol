hits = blastp 1.0e-5
              (load_faa "examples/sequences/Mycoplasma_genitalium_M2321_5genes.faa")
              (gbk_to_faa "cds" (load_gbk "examples/sequences/Mycoplasma_genitalium_M2321.gbk"))
single = extract_targets hits
mapped = extract_targets_each [hits]
result = [single] | mapped
