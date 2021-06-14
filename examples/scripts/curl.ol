# Biology resources tend to bitrot quickly after a grant ends or someone
# publishes or graduates. Sometimes they also change without warning.
# That's no good for reproducability!
#
# The idea of the ortholang date functions is that they keep track of when you
# originally downloaded a file and let you manually choose to update it later
# or bundle a known good version with your script.

# This is the standard, more flexible version you should use.
# There are 3 options for the first string argument:
#   "today", which expands to the current date version above and downloads the file
#   "cached", which uses the most recent already-cached version, or defaults to "today" if there isn't one
#   an explicit date in YYYY-MM-DD format corresponding to a previous cached version
today  = curl "today"  "https://raw.githubusercontent.com/jefdaj/ortholang/master/examples/sequences/Mycoplasma_agalactiae_small.faa"
cached = curl "cached" "https://raw.githubusercontent.com/jefdaj/ortholang/master/examples/sequences/Mycoplasma_agalactiae_small.faa"

# If you give something else that doesn't fit any of those options, it will throw an error
fail = curl "this is not a valid date" "https://raw.githubusercontent.com/jefdaj/ortholang/master/examples/sequences/Mycoplasma_agalactiae_small.faa"

# Files downloaded with curl are "untyped" until you use a load function to tell ortholang how to handle them:
# TODO write the code for loading untyped files?
# today_faa  = load_faa today
# cached_faa = load_faa cached

# Files downloaded with other dated functions, like blast_db_get_*, already include their corresponding load functions.

# Starting from a blank cache, these should both point to the same file with today's date.
result = [today, cached]
