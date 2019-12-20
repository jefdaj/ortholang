all_faas = load_faa_glob "examples/sequences/*.faa"
small_faas = load_faa_glob "examples/sequences/*_small.faa"
result = all_faas ~ small_faas
