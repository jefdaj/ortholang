# This runs custom-r-filter.R to demonstrate filtering BLAST hits and passing
# the result to another OrthoLang function. See also r_plots.ol for a more
# complicated example of using variable names in plots.

# Do a regular forward search to get some hits first.
s1 = load_faa "examples/sequences/Mycoplasma_gallisepticum_protein_refseq.faa"
s2 = load_faa "examples/sequences/Mycoplasma_pulmonis_protein_refseq.faa"
fwd_hits = blastp 1e-20 s1 s2

# Filter the hits using a custom script that only keeps the genes we're
# interested in. Note that this returns an 'untyped' result, not 'bht' as you
# might expect. OrthoLang will trust you know what you're doing, and blindly
# pass the 'untyped' value to any function. But if the formats don't match up,
# that function will probably crash or return wrong results.
fwd_hits_filtered = run_script "examples/scripts/custom-r-filter.R" [fwd_hits]

# In this case, we know the format will still be valid because all we did is
# remove some lines from the table. So we can go ahead and use it as if it were
# a 'bht'. And once the next function has been run, its result will be locked
# in to a specific type ('str.list' in this case) as usual.
target_ids = extract_targets fwd_hits_filtered
s2_small = extract_seqs s2 target_ids

# Now we'll make a smaller FASTA file with only the relevant genes to speed up
# the reverse search. Note that this is just a code example, not necessarily a
# smart way to set up a reciprocal best hits search.
rev_hits = blastp 1e-20 s2_small s1

# And finally, use the two hit tables to find reciprocal best hits as usual.
rb_hits = reciprocal_best fwd_hits rev_hits
result = rb_hits
