small = load_faa_glob "examples/sequences/Mycoplasma_*_refseq.faa"
ofres = orthofinder (sample 5 small)
og662 = orthogroup_containing ofres "WP_013729662.1"
result = og662
