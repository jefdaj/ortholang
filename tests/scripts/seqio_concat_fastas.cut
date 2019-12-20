single = concat_fna [gbk_to_fna "cds" (load_gbk "examples/sequences/Mycoplasma_genitalium_M2321.gbk"),
                     gbk_to_fna "cds" (load_gbk "examples/sequences/Mycoplasma_canadense_HAZ360_1.gbk")]
mapped = concat_fna_each [[gbk_to_fna "cds" (load_gbk "examples/sequences/Mycoplasma_genitalium_M2321.gbk"),
                           gbk_to_fna "cds" (load_gbk "examples/sequences/Mycoplasma_canadense_HAZ360_1.gbk")]]
result = [single] | mapped
