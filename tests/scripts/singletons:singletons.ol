# Tests that the singleton functions are working properly

# Load some literal values
# TODO store nums without the decimals when they read that way
nums = [1, 2]
strs = ["1.0", "2.0", "3.0"]

# And some path-based values (any non-literal type)
cyano_paths = glob_files "examples/sequences/PCC*genes.faa"

# These fasta files shouldn't actually need to be loaded. OrthoLang just needs
# to know what their paths would be if it did load them. Isn't laziness fun??
cyano_faas = load_faa_each cyano_paths

# And an empty list, just in case of regressions
emt = []

# Make singletons
emt_singles = singletons emt
num_singles = singletons nums
str_singles = singletons strs
faa_singles = singletons cyano_faas

# Check their lengths and lengths of lengths are right...

# This should be [0,2,3,4]:
emt_len = length emt_singles
num_len = length num_singles
str_len = length str_singles
faa_len = length faa_singles
lens = [emt_len, num_len, str_len, faa_len]

# And this should 0, 2, 3, 4-long lists of [1]s:
emt_le = length_each emt_singles
num_le = length_each num_singles
str_le = length_each str_singles
faa_le = length_each faa_singles
lens_each = [emt_le, num_le, str_le, faa_le]

# This should be 2 [0,2,3,4]s:
both = [lens, length_each lens_each]
result = both
