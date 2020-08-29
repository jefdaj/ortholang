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

result = a6
