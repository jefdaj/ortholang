single = load_faa "examples/sequences/Mycoplasma_genitalium_single.faa"

# TODO find the bug! this works, but not without the variable assignment
mylist = ["examples/sequences/Mycoplasma_genitalium_single.faa"]

mapped = load_faa_each mylist
result = [single] | mapped
