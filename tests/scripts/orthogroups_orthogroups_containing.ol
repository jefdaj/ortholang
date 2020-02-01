small5 = load_faa_each (sample 5 (glob_files "examples/sequences/Mycoplasma_*_refseq.faa"))
ofres = orthofinder small5
ogs = orthogroups_containing ofres ["WP_010925290.1", "NP_975756.1", "WP_011949581.1"]
result = ogs
