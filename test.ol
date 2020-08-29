# This tests the zip_archive function.

# works except the name isn't being picked up
s1 = "first, a single string (text file)"
a1 = zip_archive [s1]

# works
a2 = zip_archive ["same, but with no variable name"]

# works 
s2 = "how about two strings?"
a3 = zip_archive [s1, s2]

# works
a4 = zip_archive [s1, s2, "how about two strings and another unnamed one?"]

# or nothing at all
# works
a5 = zip_archive []

# finally, what about recursively zipping zip files?
# almost works, but needs to learn the extension; pass explicitly?
a6 = zip_archive [a1, a2, a3, a4, a5]

# now all the same things, but with nums
n1 = 34
a7 = zip_archive [1e-45]
a8 = zip_archive [n1]
a9 = zip_archive [n1, 2]
n2 = 4
a10 = zip_archive [n1, n2]
a11 = zip_archive [a7, a8, a9, a10]

result = zip_archive [a6, a11]
