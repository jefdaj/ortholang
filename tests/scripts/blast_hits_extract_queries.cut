hits = blastp 1.0e-5
              (load_faa "examples/sequences/Mycoplasma_genitalium_small.faa")
              (load_faa "examples/sequences/Mycoplasma_agalactiae_small.faa")
single = extract_queries hits
mapped = extract_queries_each [hits]
result = [single] | mapped
