nums = [1, 2, 3]
strs = ["1", "2", "3"]
genes5 = load_faa "examples/sequences/Mycoplasma_genitalium_M2321_5genes.faa"
mgen = gbk_to_faa "CDS" (load_gbk "examples/sequences/Mycoplasma_genitalium_M2321.gbk")
dbs = makeblastdb_prot_each [genes5, mgen]
result = [length_each (singletons nums),
          length_each (singletons strs),
          length_each (singletons dbs)]
