# This tests the zip_archive function.

# First, can we zip a single plain text file?
s1 = "this is a string"
a1 = zip_archive [s1]

# How about a couple of them?
s2 = "and this is another string"
a2 = zip_archive [s1, s2]

# How about when one of them doesn't have a name?
a3 = zip_archive [s1, s2, "an unnamed string for archive 3"]

result = a1
