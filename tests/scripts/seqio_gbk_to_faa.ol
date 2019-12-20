gbk = load_gbk "examples/sequences/Mycoplasma_genitalium_M2321.gbk"
faa = gbk_to_faa "cds" gbk
result = faa
