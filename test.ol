# This tests the zip_archive function.
# works except the name isn't being picked up
s1 = "first, a single string (text file)"
a1 = zip_archive [s1]

a2 = zip_archive ["same, but with no variable name"]

s2 = "how about two strings?"
a3 = zip_archive [s1, s2]

a4 = zip_archive [s1, s2, "how about two strings and another unnamed one?"]

# or nothing at all
a5 = zip_archive []

# finally, what about recursively zipping zip files?
a6 = zip_archive [a1, a2, a3, a4, a5]

# now all the same things, but with nums
n1 = 34
a7 = zip_archive [1e-45]
a8 = zip_archive [n1]
a9 = zip_archive [n1, 2]
n2 = 4
a10 = zip_archive [n1, n2]
a11 = zip_archive [a7, a8, a9, a10]

# and with a few BLAST-related types
f1 = load_faa "examples/sequences/Mycoplasma_gallisepticum_protein_refseq.faa"
f2 = load_faa "examples/sequences/Mycoplasma_pulmonis_protein_refseq.faa"
a12 = zip_archive [f1]
a13 = zip_archive [f2, f1]

hits = blastp 1e-20 f1 f2
a14 = zip_archive [hits]
a15 = zip_archive [f1, f2, hits]
a16 = zip_archive [load_faa "examples/sequences/Mycoplasma_pulmonis_protein_refseq.faa",
                   blastp 1e-20 f1 f2]
a17 = zip_archive [a14, a16, a15]

# and finally nested lists of all of the above
l1 = [n1, n2]
a18 = zip_archive [l1] # TODO this is the only broken one, because it expects a dir but creates num.list
# TODO add a similar str.list test too

l2 = [a7, a8, a9]
a19 = zip_archive [l2]
a20 = zip_archive [a7, a8, a9]

l3 = [f1, f2]
a21 = zip_archive [l3]

l4 = [hits]
a22 = zip_archive [[hits]]
a23 = zip_archive [l4]

a24 = zip_archive [a19, a20, a21, a22, a23]

result = zip_archive [a6, a11, a17, a24]
